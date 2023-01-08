module Auth.Get (getUser) where

import Auth.JWT (authorizedUserId)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Hasql.Pool as HP
import qualified Hasql.Session as HS
import qualified Hasql.Statement as S
import qualified Hasql.TH as TH
import Network.HTTP.Types.Status (status500)
import Web.Scotty
  ( ActionM,
    ScottyM,
    get,
    json,
    raiseStatus,
  )

data GetResponse = GetResponse
  { id :: Int,
    username :: String,
    email :: String
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''GetResponse)

selectUser :: S.Statement Int64 (T.Text, T.Text, Bool)
selectUser =
  [TH.singletonStatement|
    select
    username :: text, email :: text, is_active :: bool
    from auth_user
    where id = $1 :: int8
  |]

authorizedUser :: HP.Pool -> ActionM GetResponse
authorizedUser pool = do
  userId <- authorizedUserId
  let sess = HS.statement userId selectUser
  result <- liftIO $ HP.use pool sess
  case result of
    Right (username, email, isActive) -> do
      return GetResponse {id = fromIntegral userId, username = T.unpack username, email = T.unpack email}
    Left err -> raiseStatus status500 "disaster usin the DB"

getUser :: HP.Pool -> ScottyM ()
getUser pool = get "/api/user/" $ do
  user <- authorizedUser pool
  json user
