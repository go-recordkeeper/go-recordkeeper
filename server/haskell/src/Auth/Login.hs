{-# LANGUAGE QuasiQuotes #-}

module Auth.Login (login) where

-- import Auth.User
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Password.PBKDF2 (PasswordCheck (..), PasswordHash (..), checkPassword, mkPassword)
import Data.Text (Text, pack, splitOn, unpack)
import Data.Text.Encoding.Base64 (encodeBase64)
import qualified Hasql.Pool as HP
import qualified Hasql.Session as HS
import qualified Hasql.Statement as S
import qualified Hasql.TH as TH
import Network.HTTP.Types (status200, status401)
import Web.Scotty
  ( ActionM,
    ScottyM,
    json,
    jsonData,
    post,
    raiseStatus,
    status,
  )

data LoginRequest = LoginRequest
  { username :: String,
    password :: String
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''LoginRequest)

getUser :: S.Statement Text (Text, Bool)
getUser =
  [TH.singletonStatement|
    select
    password :: text, is_active :: bool
    from auth_user
    where username = $1 :: text
  |]

login :: HP.Pool -> ScottyM ()
login pool = post "/api/login/" $ do
  LoginRequest {username, password} <- jsonData :: ActionM LoginRequest
  let password' = mkPassword $ pack password
  let sess = HS.statement (pack username) getUser
  result <- liftIO $ HP.use pool sess
  case result of
    Right (passwordHash, isActive) -> do
      if isActive
        then do
          -- The reference implementation uses a different hash storage format, so we need to convert
          let hashParts = splitOn "$" passwordHash
          let salt = unpack $ encodeBase64 $ hashParts !! 2
          let hash = unpack $ hashParts !! 3
          let haskellHash = pack $ "sha256:390000:" ++ salt ++ ":" ++ hash
          let passwordHash' = PasswordHash {unPasswordHash = haskellHash}
          case checkPassword password' passwordHash' of
            PasswordCheckSuccess -> do
              status status200
              -- TODO return a valid JWT
              json $ pack "GUCCI"
            -- TODO proper error messages
            PasswordCheckFail -> raiseStatus status401 "sus"
        else raiseStatus status401 "sus"
    Left err -> raiseStatus status401 "sus"
