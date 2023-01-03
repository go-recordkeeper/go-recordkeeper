{-# LANGUAGE QuasiQuotes #-}

module Auth.Login (login) where

-- import Auth.User
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.TH (defaultOptions, deriveJSON)
-- import Data.Password.PBKDF2 (PBKDF2Algorithm (PBKDF2_SHA256), PBKDF2Params (PBKDF2Params, pbkdf2Algorithm, pbkdf2Iterations, pbkdf2OutputLength, pbkdf2Salt), PasswordCheck (PasswordCheckFail, PasswordCheckSuccess), PasswordHash (PasswordHash, unPasswordHash), checkPassword, hashPasswordWithParams, mkPassword)

-- import Data.Password.PBKDF2 (PBKDF2Algorithm (PBKDF2_SHA256), PBKDF2Params (PBKDF2Params, pbkdf2Algorithm, pbkdf2Iterations, pbkdf2OutputLength, pbkdf2Salt), PasswordCheck (PasswordCheckFail, PasswordCheckSuccess), PasswordHash (PasswordHash, unPasswordHash), checkPassword, hashPasswordWithParams, mkPassword)
-- import Data.Password.PBKDF2 (PBKDF2Algorithm (PBKDF2_SHA256), PBKDF2Params (PBKDF2Params, pbkdf2Algorithm, pbkdf2Iterations, pbkdf2OutputLength, pbkdf2Salt), PasswordCheck (PasswordCheckFail, PasswordCheckSuccess), PasswordHash (PasswordHash, unPasswordHash), checkPassword, hashPasswordWithParams, mkPassword)

-- import Data.Password.PBKDF2 (PBKDF2Algorithm (PBKDF2_SHA256), PBKDF2Params (PBKDF2Params, pbkdf2Algorithm, pbkdf2Iterations, pbkdf2OutputLength, pbkdf2Salt), PasswordCheck (PasswordCheckFail, PasswordCheckSuccess), PasswordHash (PasswordHash, unPasswordHash), checkPassword, hashPasswordWithParams, mkPassword)
-- import Data.Password.PBKDF2 (PBKDF2Algorithm (PBKDF2_SHA256), PBKDF2Params (PBKDF2Params, pbkdf2Algorithm, pbkdf2Iterations, pbkdf2OutputLength, pbkdf2Salt), PasswordCheck (PasswordCheckFail, PasswordCheckSuccess), PasswordHash (PasswordHash, unPasswordHash), checkPassword, hashPasswordWithParams, mkPassword)

-- import Data.Password.PBKDF2 (PBKDF2Algorithm (PBKDF2_SHA256), PBKDF2Params (PBKDF2Params, pbkdf2Algorithm, pbkdf2Iterations, pbkdf2OutputLength, pbkdf2Salt), PasswordCheck (PasswordCheckFail, PasswordCheckSuccess), PasswordHash (PasswordHash, unPasswordHash), checkPassword, hashPasswordWithParams, mkPassword)
-- import Data.Password.PBKDF2 (PBKDF2Algorithm (PBKDF2_SHA256), PBKDF2Params (PBKDF2Params, pbkdf2Algorithm, pbkdf2Iterations, pbkdf2OutputLength, pbkdf2Salt), PasswordCheck (PasswordCheckFail, PasswordCheckSuccess), PasswordHash (PasswordHash, unPasswordHash), checkPassword, hashPasswordWithParams, mkPassword)

-- import Data.Password.PBKDF2 (PBKDF2Algorithm (PBKDF2_SHA256), PBKDF2Params (PBKDF2Params, pbkdf2Algorithm, pbkdf2Iterations, pbkdf2OutputLength, pbkdf2Salt), PasswordCheck (PasswordCheckFail, PasswordCheckSuccess), PasswordHash (PasswordHash, unPasswordHash), checkPassword, hashPasswordWithParams, mkPassword)
-- import Data.Password.PBKDF2 (PBKDF2Algorithm (PBKDF2_SHA256), PBKDF2Params (PBKDF2Params, pbkdf2Algorithm, pbkdf2Iterations, pbkdf2OutputLength, pbkdf2Salt), PasswordCheck (PasswordCheckFail, PasswordCheckSuccess), PasswordHash (PasswordHash, unPasswordHash), checkPassword, hashPasswordWithParams, mkPassword)

-- import Data.Password.PBKDF2 (PBKDF2Algorithm (PBKDF2_SHA256), PBKDF2Params (PBKDF2Params, pbkdf2Algorithm, pbkdf2Iterations, pbkdf2OutputLength, pbkdf2Salt), PasswordCheck (PasswordCheckFail, PasswordCheckSuccess), PasswordHash (PasswordHash, unPasswordHash), checkPassword, hashPasswordWithParams, mkPassword)
-- import Data.Password.PBKDF2 (PBKDF2Algorithm (PBKDF2_SHA256), PBKDF2Params (PBKDF2Params, pbkdf2Algorithm, pbkdf2Iterations, pbkdf2OutputLength, pbkdf2Salt), PasswordCheck (PasswordCheckFail, PasswordCheckSuccess), PasswordHash (PasswordHash, unPasswordHash), checkPassword, hashPasswordWithParams, mkPassword)

-- import Data.Password.PBKDF2 (PBKDF2Algorithm (PBKDF2_SHA256), PBKDF2Params (PBKDF2Params, pbkdf2Algorithm, pbkdf2Iterations, pbkdf2OutputLength, pbkdf2Salt), PasswordCheck (PasswordCheckFail, PasswordCheckSuccess), PasswordHash (PasswordHash, unPasswordHash), checkPassword, hashPasswordWithParams, mkPassword)
-- import Data.Password.PBKDF2 (PBKDF2Algorithm (PBKDF2_SHA256), PBKDF2Params (PBKDF2Params, pbkdf2Algorithm, pbkdf2Iterations, pbkdf2OutputLength, pbkdf2Salt), PasswordCheck (PasswordCheckFail, PasswordCheckSuccess), PasswordHash (PasswordHash, unPasswordHash), checkPassword, hashPasswordWithParams, mkPassword)

-- import Data.Password.PBKDF2 (PBKDF2Algorithm (PBKDF2_SHA256), PBKDF2Params (PBKDF2Params, pbkdf2Algorithm, pbkdf2Iterations, pbkdf2OutputLength, pbkdf2Salt), PasswordCheck (PasswordCheckFail, PasswordCheckSuccess), PasswordHash (PasswordHash, unPasswordHash), checkPassword, hashPasswordWithParams, mkPassword)
-- import Data.Password.PBKDF2 (PBKDF2Algorithm (PBKDF2_SHA256), PBKDF2Params (PBKDF2Params, pbkdf2Algorithm, pbkdf2Iterations, pbkdf2OutputLength, pbkdf2Salt), PasswordCheck (PasswordCheckFail, PasswordCheckSuccess), PasswordHash (PasswordHash, unPasswordHash), checkPassword, hashPasswordWithParams, mkPassword)

-- import Data.Password.PBKDF2 (PBKDF2Algorithm (PBKDF2_SHA256), PBKDF2Params (PBKDF2Params, pbkdf2Algorithm, pbkdf2Iterations, pbkdf2OutputLength, pbkdf2Salt), PasswordCheck (PasswordCheckFail, PasswordCheckSuccess), PasswordHash (PasswordHash, unPasswordHash), checkPassword, hashPasswordWithParams, mkPassword)
import Data.Password.PBKDF2 (PasswordCheck (..), PasswordHash (..), checkPassword, mkPassword)
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as Lazy
import Data.Time (UTCTime)
import qualified Data.Time as Clock
import Hasql.Pool (UsageError (..))
import qualified Hasql.Pool as HP
import Hasql.Session (CommandError (..), QueryError (..))
import qualified Hasql.Session as HS
import qualified Hasql.Statement as S
import qualified Hasql.TH as TH
import Network.HTTP.Types (status200, status400, status500)
import Web.Scotty
  ( ActionM,
    ScottyM,
    jsonData,
    post,
    raiseStatus,
  )

data LoginRequest = LoginRequest
  { username :: String,
    password :: String
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''LoginRequest)

getUser :: S.Statement Text (Text, Text, Bool)
getUser =
  [TH.singletonStatement|
    select
    email :: text, password :: text, is_active :: bool
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
    Right (email, passwordHash, isActive) -> do
      liftIO $ putStrLn username
      liftIO $ putStrLn password
      liftIO $ putStrLn $ unpack email
      liftIO $ putStrLn $ unpack passwordHash
      liftIO $ print isActive
      if isActive
        then do
          let passwordHash' = PasswordHash {unPasswordHash = passwordHash}
          -- TODO checkPassword fails when given the correct password???
          case checkPassword password' passwordHash' of
            PasswordCheckSuccess -> raiseStatus status200 "GUCCI"
            PasswordCheckFail -> raiseStatus status400 "IMPOSTOR SUS"
        else raiseStatus status400 "deactivation"
    Left err -> raiseStatus status500 "booo"
