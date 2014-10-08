module IntuitDriver (
    IntuitClient(..)
  , Connection
  , IntuitDriver.init             -- because there's a name conflict with Prelude.init
  , UserAccountInfo(..)
  , AccountInfo(..)
  , getAccounts
  , Transaction(..)
  , getTransactions
  , AccountDetails(..)
  , getTransactionsAndDetails
  , readIntuitClient
  , getExistingAccounts
  ) where

import Data.ByteString.Lazy.Char8 (ByteString, pack, unpack)
import Data.Maybe (fromJust)
import Data.Time (UTCTime)
import IntuitParse ( AccountInfo(..) , Transaction(..), AccountDetails(..)
                   , accountDetails
                   , accountNameAndNumbers
                   , accountTransactions
                   , toInstitutionDetails
                   , xmlFromString)
import IntuitRest ( IntuitClient(..), AccessTokens, Credentials
                  , UserAccountInfo(..)
                  , deleteCustomer
                  , discoverAndAddAccounts
                  , getAccount
                  , getAccountTransactions
                  , getCustomerAccounts
                  , getInstitutionDetails
                  , readIntuitClient
                  , updateInstitutionLogin
                  , userAccessTokens
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

getTransactions :: Connection -> String -> UTCTime -> IO [Transaction]
getTransactions (C ma cre) accountId since = do
  transactionBs <- getAccountTransactions accountId since cre ma
  accountTransactions $ xmlFromString $ unpack transactionBs

getTransactionsAndDetails :: Connection -> String -> UTCTime -> 
                              IO ([Transaction], AccountDetails)
getTransactionsAndDetails (C ma cre) accountId since = do
  transactionBs <- getAccountTransactions accountId since cre ma
  transactions <- accountTransactions $ xmlFromString $ unpack transactionBs
  accountBs <- getAccount accountId cre ma
  accounts <- accountDetails $ xmlFromString $ unpack accountBs
  return (transactions, head accounts)

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
