module Auth.User (User (..), LoginRequired, LoggedInUser, requireLogin) where

import Data.Aeson
import Data.Aeson.TH
import Servant
import Servant.Auth.Server

data User = User
  { id' :: Int,
    name :: String,
    email :: String,
    password :: String
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

instance ToJWT User

instance FromJWT User

type LoginRequired = Servant.Auth.Server.Auth '[JWT] User

type LoggedInUser = Servant.Auth.Server.AuthResult User

requireLogin :: (User -> Handler getResponse) -> (LoggedInUser -> Handler getResponse)
requireLogin handler = helper'
  where
    helper' (Servant.Auth.Server.Authenticated user) = handler user
    helper' _ = throwAll err401
