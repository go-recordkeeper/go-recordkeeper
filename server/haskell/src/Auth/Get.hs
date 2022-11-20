module Auth.Get (getUser) where

import Auth.User
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Web.Scotty

data GetResponse = GetResponse
  { id :: Int,
    username :: String,
    email :: String
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''GetResponse)

-- type GetAPI = LoginRequired :> "user" :> Get '[JSON] GetResponse

-- get :: Server GetAPI
-- get = requireLogin $ \(User {id', name, email}) -> return $ GetResponse id' name email

getUser :: ScottyM ()
getUser = get "/api/user/" $ do
  -- TODO auth token
  json (User {id' = 2, name = "foo", email = "bar", password = "pass"})