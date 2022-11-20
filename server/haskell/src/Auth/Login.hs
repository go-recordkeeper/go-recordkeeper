module Auth.Login (login) where

import Auth.User
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Web.Scotty

data LoginRequest = Request
  { username :: String,
    password :: String
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''LoginRequest)

-- type LoginAPI = "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] String

-- login :: Server LoginAPI
-- login _ = return "heh token"
login :: ScottyM ()
login = get "/api/login/" $ json (User {id' = 2, name = "foo", email = "bar", password = "pass"})