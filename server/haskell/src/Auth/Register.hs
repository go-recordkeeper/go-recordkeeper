module Auth.Register (register) where

import Auth.User
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Web.Scotty

data RegisterRequest = RegisterRequest
  { username :: String,
    email :: String,
    password :: String
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''RegisterRequest)

data RegisterResponse = RegisterResponse
  { id :: Int,
    username :: String,
    email :: String
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''RegisterResponse)

-- type RegisterAPI = "register" :> ReqBody '[JSON] RegisterRequest :> PostCreated '[JSON] RegisterResponse

-- register :: Server RegisterAPI
-- register RegisterRequest {username, email} = return $ RegisterResponse 0 username email

register :: ScottyM ()
register = get "/api/register/" $ do
  -- TODO parse body
  json (User {id' = 2, name = "foo", email = "bar", password = "pass"})