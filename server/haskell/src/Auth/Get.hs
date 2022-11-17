module Auth.Get (GetAPI, get) where

import Data.Aeson
import Data.Aeson.TH
import Servant

data GetResponse = GetResponse
  { id :: Int,
    username :: String,
    email :: String
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''GetResponse)

type GetAPI = "user" :> Post '[JSON] GetResponse

get :: GetResponse
get = GetResponse 1 "daniel" "daniel@chiquit.ooo"
