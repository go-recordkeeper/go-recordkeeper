module Auth.Get (GetAPI, get) where

import Auth.User (User, email, id', name)
import Data.Aeson
import Data.Aeson.TH
import Servant
import Servant.Auth.Server

data GetResponse = GetResponse
  { id :: Int,
    username :: String,
    email :: String
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''GetResponse)

type GetAPI auths = Servant.Auth.Server.Auth auths User :> "user" :> Get '[JSON] GetResponse

get :: Servant.Auth.Server.AuthResult User -> Handler GetResponse
-- get auths :: Server GetAPI auths
get (Servant.Auth.Server.Authenticated user) = return $ GetResponse (id' user) (name user) ((email :: User -> String) user)
get _ = throwAll err401