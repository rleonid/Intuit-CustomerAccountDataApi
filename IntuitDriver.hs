module IntuitDriver ( 
  IntuitClient(..)
  , Connection
  , AccountInfo(..)
  , IntuitDriver.init
  , getAccounts
  , readIntuitClient 
  ) where

import Data.Maybe (fromJust)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import IntuitParse (AccountInfo(..), toInstitutionDetails
                   , accountNameAndNumbers
                   , xmlFromString)
import IntuitRest (IntuitClient(..), AccessTokens, Credentials
                  , UserAccountInfo(..)
                  , discoverAndAddAccounts
                  , getInstitutionDetails
                  , readIntuitClient
                  , userAccessTokens)
import Network.HTTP.Conduit (Manager, newManager, conduitManagerSettings)

data Connection = C { manager     :: Manager
                    , credentials :: Credentials
                    }

init :: IntuitClient -> String -> IO Connection
init ic client = do
  manager <- newManager conduitManagerSettings
  macctok <- userAccessTokens manager ic $ pack client
  let act = fromJust macctok                -- TODO
  return $ C manager (ic, act)
  
getAccounts :: Connection -> String -> String -> String -> IO [AccountInfo]
getAccounts (C ma cre) user pass iid = do  
  detailBs <- getInstitutionDetails iid cre ma
  detailsR <- toInstitutionDetails $ xmlFromString $ unpack detailBs
  let Right details = detailsR              -- TODO
      userAi  = UAI user pass details
  accountBs <- discoverAndAddAccounts cre ma userAi
  accountNameAndNumbers $ xmlFromString $ unpack accountBs

