module Auth.Register (register) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import qualified Data.ByteString.Char8 as BS
import Data.Int (Int64)
import Data.Password.PBKDF2 (PBKDF2, PBKDF2Algorithm (PBKDF2_SHA256), PBKDF2Params (PBKDF2Params, pbkdf2Algorithm, pbkdf2Iterations, pbkdf2OutputLength, pbkdf2Salt), PasswordHash (unPasswordHash), Salt (Salt, getSalt), hashPasswordWithSalt, mkPassword)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time (UTCTime)
import qualified Data.Time as Clock
import qualified Hasql.Pool as HP
import qualified Hasql.Session as HS
import qualified Hasql.Statement as S
import qualified Hasql.TH as TH
import Network.HTTP.Types (status201, status400, status500)
import System.Random (Random (randomRs), newStdGen)
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

insert :: S.Statement (T.Text, T.Text, T.Text, UTCTime) Int64
insert =
  [TH.rowsAffectedStatement|
    insert into auth_user
    (username, email, password, date_joined, last_login, first_name, last_name, is_superuser, is_staff, is_active)
    values
    ($1 :: text, $2 :: text, $3 :: text, $4 :: timestamptz, $4 :: timestamptz, '', '', false, false, true)
  |]

generateSalt :: IO (Salt PBKDF2)
generateSalt = do
  g <- newStdGen
  let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  let ns = randomRs (0, length chars - 1) g
  let strs = map (chars !!) ns
  return $ Salt $ BS.pack $ take 16 strs

hashPassword :: String -> IO String
hashPassword password = do
  let params = PBKDF2Params {pbkdf2Salt = 16, pbkdf2Algorithm = PBKDF2_SHA256, pbkdf2Iterations = 390000, pbkdf2OutputLength = 64}
  salt <- generateSalt
  let haskellHash = hashPasswordWithSalt params salt $ mkPassword $ T.pack password
  -- The reference implementation uses a different hash storage format, so we need to convert
  let hashParts = T.splitOn ":" $ unPasswordHash haskellHash
  let hash = T.unpack $ hashParts !! 3
  let referenceHash = "pbkdf2_sha256$390000$" ++ BS.unpack (getSalt salt) ++ "$" ++ hash
  return referenceHash

register :: HP.Pool -> ScottyM ()
register pool = post "/api/register/" $ do
  RegisterRequest {username, email, password} <- jsonData :: ActionM RegisterRequest
  if not $ isValid $ BS.pack email
    then raiseStatus status400 "Invalid email"
    else do
      passwordHash <- liftAndCatchIO $ hashPassword password
      now <- liftAndCatchIO Clock.getCurrentTime
      let sess = HS.statement (T.pack username, T.pack email, T.pack passwordHash, now) insert
      result <- liftIO $ HP.use pool sess
      case result of
        Right id' -> do
          status status201
          json (RegisterResponse {id = fromIntegral id', username, email})
        Left err ->
          case err of
            HP.SessionError (HS.QueryError _ _ (HS.ResultError (HS.ServerError "23505" _ _ _))) -> raiseStatus status400 "A user with that username already exists."
            _ -> raiseStatus status500 $ TL.pack $ show err
