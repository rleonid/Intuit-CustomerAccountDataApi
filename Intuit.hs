module Intuit (
  ppXmlToFile
  , xmlFromFile
  , xmlFromString
  , justInstitutions
  , institutionNameContains 
  , toInstitutionDetails 
  , AccountInfo(..)
  , accountNameAndNumbers 
  , AccountDetails(..)
  , accountDetails
  , Transaction(..)
  , accountTransactions
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Char (toLower)
import Data.Maybe (fromJust)
import Data.List (isInfixOf)

import Data.Time (parseTime, UTCTime)
import System.Locale (defaultTimeLocale)
import Text.Printf (printf)
import Text.XML.Light.Input (parseXMLDoc)
import Text.XML.Light.Output (ppTopElement)
import Text.XML.Light.Lexer (XmlSource)

import Text.XML.HXT.Core

import IntuitRest

--manager <- liftIO $ newManager conduitManagerSettings

ppXmlToFile :: XmlSource s => FilePath -> s -> IO ()
ppXmlToFile filename xmlStr =
  writeFile filename $ ppTopElement $ fromJust $ parseXMLDoc xmlStr

-- not as good.
--ppXmlToFile2 :: FilePath = IO ()
--ppXmlToFile2 filename xmlStr = do
--  res <- runX . xshow $ readString [] xmlStr
--  mapM_ putStrLn res

--outsideOne :: String -> UserBankCredentials -> IO ByteString
--outsideOne userId credential = do
--  mats <- accessTokens userId
--  let ats = fromJust mats
--  discoverAndAddAccounts ats credential

-- We seem to do a lot of asking of the text contained inside of a Node
-- <foo>THIS</foo> this method seems the most concise
--getChildText :: IOSLA (XIOState ()) (Data.Tree.NTree.TypeDefs.NTree XNode) String
getChildText :: ArrowXml a => a XmlTree String
getChildText = getChildren >>> getText

type LoadedXml = IOStateArrow () XmlTree XmlTree

xmlFromFile :: FilePath -> LoadedXml
xmlFromFile = readDocument [] 

xmlFromString :: String -> LoadedXml
xmlFromString = readString [] 

justInstitutions :: LoadedXml -> IO [(String,String)]
justInstitutions xml = 
  do runX (
      xml
      >>> selectInstitutions
      >>> selectIdAndName
      )
  where selectInstitutions = deep (isElem >>> hasName "institution")
        selectId           = deep (isElem >>> hasName "institutionId" >>> getChildText)
        selectName         = deep (isElem >>> hasName "institutionName" >>> getChildText)
        selectIdAndName    = selectId &&& selectName


-- this works but is slow.
institutionNameContains :: LoadedXml -> String -> IO [(String,String)]
institutionNameContains xml str =
  do runX (
      xml
      >>> selectInstitutions
      >>> (ifA nameMatches selectIdAndName none))
  where selectInstitutions = deep (isElem >>> hasName "institution")
        nameMatches        = deep (isElem 
                                    >>> hasName "institutionName"
                                    >>> getChildren
                                    >>> hasText (isInfixOf str))
        selectId           = deep (isElem >>> hasName "institutionId" >>> getChildText)
        selectName         = deep (isElem >>> hasName "institutionName" >>> getChildText)
        selectIdAndName    = selectId &&& selectName

-- user keys from Institution details
instituteIdFromIDets :: LoadedXml -> IO (Either String String)
instituteIdFromIDets xml = do
  r <- runX $ xml >>> deep (isElem >>> hasName "institutionId" >>> getChildText)
  case r of
    []  -> return $ Left "Missing Id"
    [x] -> return $ Right x
    _   -> return $ Left "Found too many id's"

-- user keys from Institution details
userKeysFromIDets :: LoadedXml -> IO [(String,String)]
userKeysFromIDets xml =
  do runX $
      xml
      >>> selectKeys 
      >>> ifA keyDisplay this none
      >>> selectName &&& selectDesc
  where selectKeys  = deep (isElem >>> hasName "key")
        keyDisplay  = getChildren >>> hasName "displayFlag" >>> getChildren >>> hasText ((==) "true")
        selectName  = getChildren >>> hasName "name" >>> getChildText
        selectDesc  = getChildren >>> hasName "description" >>> getChildText

--TODO monadize the either logic
toInstitutionDetails :: LoadedXml -> IO (Either String InstitutionDetails)
toInstitutionDetails xml = do
  instituteId <- instituteIdFromIDets xml
  userKeys    <- userKeysFromIDets xml
  case instituteId of
    Left m    -> return $ Left m
    Right iid ->
      let eitherKeys = figureOutNameAndPasswordFromKeys userKeys
      in case eitherKeys of
        Left m -> return $ Left m
        Right (passwordKey, usernameKey) ->
          return $ Right $ InstDet iid usernameKey passwordKey

figureOutNameAndPasswordFromKeys :: [(String,String)] -> Either String (PasswordKey,UsernameKey)
figureOutNameAndPasswordFromKeys keyPairList = loweredKeys lowered
  where lower    = map toLower
        lowered  = map (\(x,y) -> (lower x, lower y)) keyPairList

loweredKeys :: [(String,String)] -> Either String (String,String)
loweredKeys ((k1,k1Desc):(k2,k2Desc):[])
        | hasPassword k1 || hasPassword k1Desc = Right (k1,k2)
        | hasPassword k2 || hasPassword k2Desc = Right (k2,k1)
        | otherwise                            = 
            Left $ printf "Can't find a password amongst key %s (%s) or %s (%s)"
                    k1 k1Desc k2 k2Desc
  where hasPassword = isInfixOf "password"
loweredKeys [(k1,k1Desc)] = Left $ printf "Only one key %s (%s)" k1 k1Desc
loweredKeys []            = Left "Empty key list"
loweredKeys lst           = Left $ printf "key list has %d (> 2) elements" $ length lst


data AccountInfo = AI { intuitId   :: String   -- I think it's actually a long 
                      , lastNumber :: String   -- last 4 digits
                      , name       :: String   -- labaled as NickName
                      }
                   deriving (Eq, Ord, Read, Show)

accountNameAndNumbers :: LoadedXml -> IO [AccountInfo]
accountNameAndNumbers xml =
  do runX $
      xml
      >>> selectAccounts
      >>> selectId &&& (selectNumber &&& selectNickName)
      >>> arr (\(iid, (num, nam)) -> AI iid num nam)
  where selectAccounts = deep (isElem >>> hasName "ns7:OtherAccount")
        selectId       = getChildren >>> hasName "accountId" >>> getChildText
        selectNumber   = getChildren >>> hasName "accountNumber" >>> getChildText
        selectNickName = getChildren >>> hasName "accountNickname" >>> getChildText

data AccountDetails = AD { currentBalance :: Double
                         , paymentDueDate :: UTCTime
                         , statementEndDate :: UTCTime
                         }
                      deriving (Eq, Ord, Read, Show)


parseUTCT :: String -> UTCTime
parseUTCT s = fromJust $ parseTime defaultTimeLocale "%FT%X%z" s

accountDetails :: LoadedXml -> IO [AccountDetails]
accountDetails xml =
  do runX $
      xml
      >>> selectAccount
      >>> selectCurBal &&& (selectDueDate &&& selectEndDate)
      >>> arr (\(b, (d, e)) -> AD (read b) (parseUTCT d) (parseUTCT e))
  where selectAccount = deep (isElem >>> hasName "ns3:CreditAccount")
        selectCurBal  = getChildren >>> hasName "ns3:currentBalance" >>> getChildText
        selectDueDate = getChildren >>> hasName "ns3:paymentDueDate" >>> getChildText
        selectEndDate = getChildren >>> hasName "ns3:statementEndDate" >>> getChildText

data Transaction = T { payeeName  :: String 
                     , postedDate :: UTCTime
                     , amount     :: Double
                     }
                   deriving (Eq, Ord, Read, Show)

accountTransactions :: LoadedXml -> IO [Transaction]
accountTransactions xml =
  do runX $
      xml 
      >>> selectTransactions
      >>> selectPayeeName &&& (selectPostedDate &&& selectAmount)
      >>> arr (\(p, (d, a)) -> T p (parseUTCT d) (read a))
  where selectTransactions = deep (isElem >>> hasName "ns3:CreditCardTransaction")
        selectPayeeName    = getChildren >>> hasName "payeeName" >>> getChildText
        selectPostedDate   = getChildren >>> hasName "postedDate" >>> getChildText
        selectAmount       = getChildren >>> hasName "amount" >>> getChildText

-- Flow
-- X 1: User select instiution name
-- X 2. We get institution details and display keys
-- X 3. User specifies account username/password
-- X 4. We login and fetch accounts
-- X 5. User specifies [Credit Card | Funding] account.
-- 6. We fetch transactions, balance and due date for CC accounts.
