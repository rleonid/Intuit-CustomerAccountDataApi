module IntuitDriver (
  IntuitClient(..)
  , Connection
  , AccountInfo(..)
  , IntuitDriver.init
  , getAccounts
  , readIntuitClient
  , getExistingAccounts
  ) where

import Data.Maybe (fromJust)
import Data.ByteString.Lazy.Char8 (ByteString, pack, unpack)
import IntuitParse (AccountInfo(..), toInstitutionDetails
                   , accountNameAndNumbers
                   , xmlFromString)
import IntuitRest (IntuitClient(..), AccessTokens, Credentials
                  , UserAccountInfo(..)
                  , discoverAndAddAccounts
                  , getInstitutionDetails
                  , readIntuitClient
                  , userAccessTokens
                  , updateInstitutionLogin
                  , getCustomerAccounts
                  , deleteCustomer
                  )
import Network.HTTP.Conduit (Manager, newManager, conduitManagerSettings)

data Connection = C { manager     :: Manager
                    , credentials :: Credentials
                    }

-- Initialize a per user Connection instance for retrieving data from Intuit
init :: IntuitClient -> String -> IO Connection
init ic client = do
  manager <- newManager conduitManagerSettings
  macctok <- userAccessTokens manager ic $ pack client
  let act = fromJust macctok                -- TODO
  --print act
  return $ C manager (ic, act)

-- Get the accounts associated
getAccounts :: Connection -> UserAccountInfo String -> IO [AccountInfo]
getAccounts (C ma cre) (UserAccountInfo user pass iid) = do
  detailBs <- getInstitutionDetails iid cre ma
  --print detailBs
  detailsR <- toInstitutionDetails $ xmlFromString $ unpack detailBs
  let Right details = detailsR              -- TODO
      userAi  = UserAccountInfo user pass details
  accountBs <- discoverAndAddAccounts cre ma userAi
  accountNameAndNumbers $ xmlFromString $ unpack accountBs

-------------------
-- Etc methods
-------------------
updateLogin :: Connection -> String -> String -> String -> IO ByteString
updateLogin (C ma cre) user pass iid = do
  detailBs <- getInstitutionDetails iid cre ma
  detailsR <- toInstitutionDetails $ xmlFromString $ unpack detailBs
  let Right details = detailsR              -- TODO
      userAi  = UserAccountInfo user pass details
  --print userAi
  updateInstitutionLogin cre ma userAi

getExistingAccounts :: Connection -> IO ByteString
getExistingAccounts (C ma cre) =
  getCustomerAccounts cre ma

deleteUser :: Connection -> IO ByteString
deleteUser (C ma cre) =
  deleteCustomer cre ma
