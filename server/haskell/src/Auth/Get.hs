module Auth.Get (GetAPI, get) where

import Auth.User
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

type GetAPI = LoginRequired :> "user" :> Get '[JSON] GetResponse

get :: Server GetAPI
get = requireLogin $ \(User {id', name, email}) -> return $ GetResponse id' name email