module Auth.User (User (..)) where

import Data.Aeson
import Data.Aeson.TH
import Servant.Auth.Server (FromJWT, ToJWT)

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
