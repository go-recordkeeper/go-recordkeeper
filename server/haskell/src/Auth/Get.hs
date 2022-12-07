module Auth.Get (getUser) where

import Auth.User
import Data.Aeson.TH (defaultOptions, deriveJSON)
import qualified Data.Text.Lazy as DT
import qualified Hasql.Connection as HC
import qualified Hasql.Session as HS
import Network.HTTP.Types.Status (status200, status401)
import Web.Scotty

data GetResponse = GetResponse
  { id :: Int,
    username :: String,
    email :: String
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''GetResponse)

xtractr :: ActionM DT.Text
xtractr = do
  headerValue <- header "Authorization"
  let token = DT.stripPrefix "Bearer " =<< headerValue
  maybe (raiseStatus status401 "Not Authorized") pure token

getUser :: ScottyM ()
getUser = get "/api/user/" $ do
  -- TODO auth token
  foo <- xtractr
  json (User {id' = 2, name = DT.unpack foo, email = "bar", password = "pass"})