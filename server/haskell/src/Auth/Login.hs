{-# LANGUAGE QuasiQuotes #-}

module Auth.Login (login) where

-- import Auth.User

import Control.Lens (set, view)
import Control.Lens.Combinators (re)
import Control.Lens.Operators
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Crypto.JWT hiding (hash, jwk)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import qualified Data.ByteString.Lazy as BSL
import Data.Int (Int64)
import Data.Password.PBKDF2 (PasswordCheck (..), PasswordHash (..), checkPassword, mkPassword)
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Base64 as T
import qualified Data.Text.Strict.Lens as T
import Data.Time (UTCTime, addUTCTime, getCurrentTime, nominalDay)
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

getUser :: S.Statement T.Text (Int64, T.Text, Bool)
getUser =
  [TH.singletonStatement|
    select
    id :: int8, password :: text, is_active :: bool
    from auth_user
    where username = $1 :: text
  |]

-- The reference application stores hashed passwords like this:
-- `pbkdf2_sha256$390000$plaintextsalt$hashedpassword`
-- The password package generates hashed passwords that look like this:
-- `sha256:390000:base64encodedsalt==:hashedpassword`
-- Convert the reference style into the haskell style
convertPasswordHash :: T.Text -> PasswordHash a
convertPasswordHash passwordHash = do
  let hashParts = T.splitOn "$" passwordHash
  let salt = T.unpack $ T.encodeBase64 $ hashParts !! 2
  let hash = T.unpack $ hashParts !! 3
  let haskellHash = T.pack $ "sha256:390000:" ++ salt ++ ":" ++ hash
  PasswordHash {unPasswordHash = haskellHash}

mkClaims :: Int64 -> UTCTime -> ClaimsSet
mkClaims id' currentTime = do
  -- currentTime <- getCurrentTime
  let expiration = addUTCTime nominalDay currentTime
  emptyClaimsSet
    & claimSub ?~ fromString (show id')
    & claimIat ?~ NumericDate currentTime
    & claimExp ?~ NumericDate expiration
    & claimIss ?~ "go-recordkeeper"
    & claimAud ?~ Audience ["go-recordkeeper"]

-- TODO delete this, load the JWK from the env var SECRET_KEY instead
generateJWK :: IO JWK
generateJWK = do
  jwk <- genJWK (OctGenParam (4096 `div` 8))
  let h = view thumbprint jwk :: Digest SHA256
      kid' = view (re (base64url . digest) . T.utf8) h
  pure $ set jwkKid (Just kid') jwk

generateStableJWK :: IO JWK
generateStableJWK = do
  let secretKey = "django-insecure-(@ppnpk$wx_z%2^#^0sext&+%b58=%e^!_u_*yd2p#d2&9)9cj"
      jwk = fromOctets $ T.encodeUtf8 secretKey
  pure jwk

signJWT :: JWK -> ClaimsSet -> IO (Either JWTError SignedJWT)
signJWT jwk claims = runExceptT $ do
  signClaims jwk (newJWSHeader ((), HS256)) claims

login :: HP.Pool -> ScottyM ()
login pool = post "/api/login/" $ do
  LoginRequest {username, password} <- jsonData :: ActionM LoginRequest
  let password' = mkPassword $ T.pack password
  let sess = HS.statement (T.pack username) getUser
  result <- liftIO $ HP.use pool sess
  case result of
    Right (id', passwordHash, isActive) -> do
      -- The reference implementation uses a different hash storage format, so we need to convert
      let passwordHash' = convertPasswordHash passwordHash
      if isActive
        then do
          case checkPassword password' passwordHash' of
            PasswordCheckSuccess -> do
              status status200
              jwk <- liftIO generateStableJWK
              now <- liftIO getCurrentTime
              maybeJWT <- liftIO $ signJWT jwk $ mkClaims id' now
              case maybeJWT of
                Right jwt -> json $ T.decodeUtf8 $ BSL.toStrict $ encodeCompact jwt
                Left err -> json $ T.pack $ show err
            -- TODO proper error messages
            PasswordCheckFail -> raiseStatus status401 "sus"
        else raiseStatus status401 "sus"
    Left err -> raiseStatus status401 "sus"
