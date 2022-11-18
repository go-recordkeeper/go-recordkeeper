module Auth.Login (login, LoginAPI) where

import Data.Aeson
import Data.Aeson.TH
import Servant

data LoginRequest = Request
  { username :: String,
    password :: String
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''LoginRequest)

type LoginAPI = "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] String

login :: Server LoginAPI
login _ = return "heh token"