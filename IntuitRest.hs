{-# LANGUAGE OverloadedStrings, QuasiQuotes, ExtendedDefaultRules #-}
module IntuitRest (
    AccessTokens
  , userAccessTokens
  , IntuitClient(..)
  , Credentials(..)
  , readIntuitClient
  , getInstitutions
  , getInstitutionDetails
  , PasswordKey
  , UsernameKey
  , InstitutionDetails(..)
  , discoverAndAddAccounts
  , getAccount
  , getAccountTransactions
  ) where

import Control.Monad (liftM)
import Data.UUID (toASCIIBytes)
import Data.UUID.V4 (nextRandom)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Crypto.PubKey.OpenSsh (decodePrivate, OpenSshPrivateKey(..))
import Crypto.Types.PubKey.RSA (PrivateKey)

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LB (filter, fromStrict, toStrict)
import Data.ByteString.Char8 as B (pack)    -- only for loading Private Key

import Data.Maybe (fromJust)
import Data.Time (formatTime, readTime, UTCTime)
import Data.Time.Clock (getCurrentTime)
import System.Locale (defaultTimeLocale)
import Text.InterpolatedString.Perl6 (qc)
import Network.HTTP.Conduit
import Network.HTTP.Types (parseSimpleQuery)
import Web.Authenticate.OAuth

import Signing

-- Generate a reference id used in our requests.
genRefenceId :: IO ByteString
genRefenceId = do
  s <- liftM toASCIIBytes nextRandom
  return $ LB.filter ((/=) '-') $ LB.fromStrict s

-- Handle reading of the private ssl Key
-- TODO perpetuate upwards the Either Monad
throwLeft :: Either String OpenSshPrivateKey -> PrivateKey
throwLeft (Right (OpenSshPrivateKeyRsa k)) = k
throwLeft (Right _) = error "Wrong key type"
throwLeft (Left s)  = error $ "Error reading keys: " ++ s

-- Read in an SSL key
loadKey :: FilePath -> IO PrivateKey
loadKey p = (throwLeft . decodePrivate . B.pack) `fmap` readFile p

data IntuitClient = IC { iOAuthConsumerKey    :: ByteString
                       , iOAuthConsumerSecret :: ByteString
                       , icSamlId             :: ByteString
                       , privateKeyFile       :: FilePath
                       }
                    deriving (Eq, Ord, Read, Show)

-- Load a client 'Show'ed to a file.
readIntuitClient :: FilePath -> IO IntuitClient
readIntuitClient file = do
  s <- readFile file
  return $ read s

data AccessTokens = AT { token  :: ByteString
                       , secret :: ByteString
                       }
                    deriving (Eq, Ord, Read, Show)

accessTokenUrl :: String
accessTokenUrl = "https://oauth.intuit.com/oauth/v1/get_access_token_by_saml"

-- Ask Intuit for a User's Access Tokens.
userAccessTokens :: Manager -> IntuitClient -> ByteString -> IO (Maybe AccessTokens)
userAccessTokens manager ic userId = do
  privateKey  <- loadKey $ privateKeyFile ic
  referenceId <- genRefenceId
  currentTime <- getCurrentTime
  let commonArgs    = C referenceId userId (icSamlId ic) currentTime
      assertionText = assertionWithSignature commonArgs privateKey
  request <- parseUrl accessTokenUrl
  let consKey   = iOAuthConsumerKey ic
      headers   = [("Authorization" , [qc|OAuth oauth_consumer_key="{consKey}"|])]
      request'  = urlEncodedBody [ ("saml_assertion", LB.toStrict assertionText) ] $
                      request { requestHeaders = headers }
  res <- httpLbs request' manager
  let resBody = responseBody res
      asAssoc = parseSimpleQuery $ LB.toStrict resBody
      mToken  = lookup "oauth_token" asAssoc
      mSecret = lookup "oauth_token_secret" asAssoc
      tokenP  = case (mToken, mSecret) of
                    (Just t, Just s) -> Just $ AT (LB.fromStrict t) (LB.fromStrict s)
                    _                -> Nothing
  return tokenP

-- Write the user tokens to file.
accessTokensToFile :: Manager -> IntuitClient -> ByteString -> FilePath -> IO ()
accessTokensToFile manager ic userId filename = do
  mAccessTokens <- userAccessTokens manager ic userId
  case mAccessTokens of
    Just at -> writeFile filename (show at)
    Nothing -> return ()

baseUrl :: String
baseUrl = "https://financialdatafeed.platform.intuit.com/v1"

type Credentials = (IntuitClient, AccessTokens)

-- Setup the OAuth creentials and application necessary for the OAuth server
setupOauth :: Credentials -> (Credential, OAuth)
setupOauth ((IC okey osecret _ _), (AT t s)) = (credential, oauthApp)
  where credential  = newCredential (LB.toStrict t) (LB.toStrict s)
        oauthApp    = newOAuth { oauthServerName      = baseUrl
                               , oauthConsumerKey     = LB.toStrict okey
                               , oauthConsumerSecret  = LB.toStrict osecret
                               }

-- measured in microseconds
maxTimeOut :: Int
maxTimeOut = 60 * (10^(6 :: Integer))     -- Haskell default casting

type ResourceName = String

getRequest :: ResourceName -> Credentials -> Manager -> IO ByteString
getRequest resourceName credentials manager = do 
  request <- parseUrl $ baseUrl ++ resourceName
  let reqTime = request { responseTimeout = Just maxTimeOut }
      (cred, oaApp) = setupOauth credentials
  signed   <- signOAuth oaApp cred reqTime
  response <- httpLbs signed manager
  return $ responseBody response
 
-- Fetch the institutions supported by Intuit
getInstitutions :: Credentials -> Manager -> IO ByteString
getInstitutions = getRequest "/institutions"

-- Get the details of an institution with the passed Intuit ID.
getInstitutionDetails :: String -> Credentials -> Manager -> IO ByteString
getInstitutionDetails institutionId = getRequest ("/institutions/" ++ institutionId)

type PasswordKey = String
type UsernameKey = String


data InstitutionDetails = InstDet { instId      :: String
                                  , usernameKey :: UsernameKey
                                  , passwordKey :: PasswordKey
                                  }
                          deriving (Eq, Ord, Read, Show)

data UserAccountInfo = UAI { username     :: String
                           , password     :: String
                           , institution  :: InstitutionDetails
                           }
                       deriving (Eq, Ord, Read, Show)

-- Construct the login XML, necessary to discover a user's accounts.
getAccountCredentialsXml :: UserAccountInfo -> ByteString
getAccountCredentialsXml (UAI username password (InstDet _ usernameKey passwordKey)) =
  [qc|<InstitutionLogin xmlns="http://schema.intuit.com/platform/fdatafeed/institutionlogin/v1"><credentials><credential><name>{usernameKey}</name><value>{username}</value></credential><credential><name>{passwordKey}</name><value>password</value></credential></credentials></InstitutionLogin>|]

-- For a given user lookup their accounts stored at the desired institution.
discoverAndAddAccounts :: Credentials -> Manager -> UserAccountInfo -> IO ByteString
discoverAndAddAccounts credentials manager uai = do
  let (cred, oaApp) = setupOauth credentials
      bodyString    = getAccountCredentialsXml uai
      iid           = instId $ institution uai
      fullUrl       = baseUrl ++ "/institutions/" ++ iid ++ "/logins"
  request <- parseUrl fullUrl
  let reqTime = request { method = "POST"
                        , responseTimeout = Just maxTimeOut
                        , requestBody  = RequestBodyLBS $ bodyString
                        }
  signed   <- signOAuth oaApp cred reqTime
  response <- httpLbs signed manager
  return $ responseBody response

-- Get account information.
getAccount :: String -> Credentials -> Manager -> IO ByteString
getAccount accountId = getRequest ("/accounts/" ++ accountId)

-- Get current transactions in an account.
getAccountTransactions :: String -> UTCTime -> Credentials -> Manager -> IO ByteString
getAccountTransactions accountId startDate = getRequest resourceName
  where startDateStr  = formatTime defaultTimeLocale "%F" startDate
        resourceName  = "/accounts/" ++ accountId ++ 
                          "/transactions?txnStartDate=" ++ startDateStr

