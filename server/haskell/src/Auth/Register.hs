{-# LANGUAGE QuasiQuotes #-}

module Auth.Register (register) where

-- import Auth.User
-- import Auth.User
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import qualified Data.ByteString.Char8 as BS
import Data.Int (Int64)
import Data.Password.PBKDF2 (PBKDF2Algorithm (PBKDF2_SHA256), PBKDF2Params (PBKDF2Params, pbkdf2Algorithm, pbkdf2Iterations, pbkdf2OutputLength, pbkdf2Salt), hashPasswordWithParams, mkPassword)
import Data.Text (Text, pack)
import qualified Data.Text.Lazy as Lazy
import Data.Time (UTCTime)
import qualified Data.Time as Clock
import Hasql.Pool (UsageError (..))
import qualified Hasql.Pool as HP
import Hasql.Session (CommandError (..), QueryError (..))
import qualified Hasql.Session as HS
import qualified Hasql.Statement as S
import qualified Hasql.TH as TH
import Network.HTTP.Types (status201, status400, status500)
import Text.Email.Validate (isValid)
import Web.Scotty
  ( ActionM,
    ScottyM,
    json,
    jsonData,
    liftAndCatchIO,
    post,
    raiseStatus,
    status,
  )

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

insert :: S.Statement (Text, Text, Text, UTCTime) Int64
insert =
  [TH.rowsAffectedStatement|
    insert into auth_user
    (username, email, password, date_joined, last_login, first_name, last_name, is_superuser, is_staff, is_active)
    values
    ($1 :: text, $2 :: text, $3 :: text, $4 :: timestamptz, $4 :: timestamptz, '', '', false, false, false)
  |]

hashPassword :: String -> IO String
hashPassword x = do
  let params = PBKDF2Params {pbkdf2Salt = 16, pbkdf2Algorithm = PBKDF2_SHA256, pbkdf2Iterations = 390000, pbkdf2OutputLength = 64}
  hash <- hashPasswordWithParams params $ mkPassword $ pack x
  return $ show hash

register :: HP.Pool -> ScottyM ()
register pool = post "/api/register/" $ do
  RegisterRequest {username, email, password} <- jsonData :: ActionM RegisterRequest
  if not $ isValid $ BS.pack email
    then raiseStatus status400 "Invalid email"
    else do
      passwordHash <- liftAndCatchIO $ hashPassword password
      now <- liftAndCatchIO Clock.getCurrentTime
      let sess = HS.statement (pack username, pack email, pack passwordHash, now) insert
      result <- liftIO $ HP.use pool sess
      case result of
        Right id' -> do
          status status201
          json (RegisterResponse {id = fromIntegral id', username, email})
        Left err ->
          case err of
            SessionError (QueryError _ _ (ResultError (HS.ServerError "23505" _ _ _))) -> raiseStatus status400 "A user with that username already exists."
            _ -> raiseStatus status500 $ Lazy.pack $ show err
