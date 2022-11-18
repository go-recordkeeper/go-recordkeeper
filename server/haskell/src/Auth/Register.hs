module Auth.Register (RegisterAPI, register) where

import Data.Aeson
import Data.Aeson.TH
import Servant

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

type RegisterAPI = "register" :> ReqBody '[JSON] RegisterRequest :> PostCreated '[JSON] RegisterResponse

register :: Server RegisterAPI
register RegisterRequest {username, email} = return $ RegisterResponse 0 username email
