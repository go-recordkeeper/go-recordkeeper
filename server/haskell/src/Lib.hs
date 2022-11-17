module Lib (startApp, app) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

data LoginRequest = LoginRequest
  { username :: String,
    password :: String
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''LoginRequest)

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

data GetUserResponse = GetUserResponse
  { id :: Int,
    username :: String,
    email :: String
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''GetUserResponse)

type API =
  "api"
    :> ( "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] String
           :<|> "register" :> ReqBody '[JSON] RegisterRequest :> PostCreated '[JSON] RegisterResponse
           :<|> "user" :> Post '[JSON] GetUserResponse
       )

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return . login :<|> return . register :<|> return user

login :: LoginRequest -> String
login LoginRequest {username, password} = "secret auth token LMAO " ++ username ++ " " ++ password

register :: RegisterRequest -> RegisterResponse
register RegisterRequest {username, email} = RegisterResponse 0 username email

user :: GetUserResponse
user = GetUserResponse 1 "daniel" "daniel@chiquit.ooo"