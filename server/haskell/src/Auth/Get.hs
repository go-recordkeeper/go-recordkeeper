{-# LANGUAGE QuasiQuotes #-}

module Auth.Get (getUser) where

import Control.Lens (view, (^?))
import Control.Monad.Except (MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Crypto.JOSE (JWK, decodeCompact)
import Crypto.JOSE.JWK (fromOctets)
import Crypto.JWT (ClaimsSet, JWTError, SignedJWT, claimSub, defaultJWTValidationSettings, string, verifyClaims)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Hasql.Pool as HP
import qualified Hasql.Session as HS
import qualified Hasql.Statement as S
import qualified Hasql.TH as TH
import Network.HTTP.Types.Status (status403, status500)
import Web.Scotty
  ( ActionM,
    ScottyM,
    get,
    header,
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

xtractr :: ActionM TL.Text
xtractr = do
  headerValue <- header "Authorization"
  let token = TL.stripPrefix "Bearer " =<< headerValue
  maybe (raiseStatus status403 "Not Authorized") pure token

selectUser :: S.Statement Int64 (T.Text, T.Text, Bool)
selectUser =
  [TH.singletonStatement|
    select
    username :: text, email :: text, is_active :: bool
    from auth_user
    where id = $1 :: int8
  |]

generateStableJWK :: IO JWK
generateStableJWK = do
  let secretKey = "django-insecure-(@ppnpk$wx_z%2^#^0sext&+%b58=%e^!_u_*yd2p#d2&9)9cj"
      jwk = fromOctets $ T.encodeUtf8 secretKey
  pure jwk

verifyJWT :: JWK -> SignedJWT -> IO (Either JWTError ClaimsSet)
verifyJWT jwk jwt = runExceptT $ do
  let config = defaultJWTValidationSettings (== "go-recordkeeper")
  verifyClaims config jwk jwt

-- authorizedUserId :: TL.Text -> IO (Either JWTError Int64)
authorizedUserId :: TL.Text -> IO (Either JWTError Int64)
authorizedUserId token = runExceptT $ do
  jwk <- liftIO generateStableJWK
  let foo = TL.encodeUtf8 token
  jwt <- decodeCompact $ TL.encodeUtf8 token
  result <- liftIO $ verifyJWT jwk jwt
  case result of
    Right claims -> do
      case view claimSub claims of
        Just id' -> do
          case id' ^? string of
            Just id'' -> return $ read $ T.unpack id''
            Nothing -> return 6666 -- TODO custom error
        Nothing -> return 6666 -- TODO custom error
    Left err -> throwError err

authorizedUser :: HP.Pool -> TL.Text -> IO (Maybe GetResponse)
authorizedUser pool token = do
  result <- liftIO $ authorizedUserId token
  case result of
    Right id -> do
      let sess = HS.statement id selectUser
      result <- liftIO $ HP.use pool sess
      case result of
        Right (username, email, isActive) -> do
          return $ Just GetResponse {id = fromIntegral id, username = T.unpack username, email = T.unpack email}
        Left err -> return Nothing
    Left err -> return Nothing

getUser :: HP.Pool -> ScottyM ()
getUser pool = get "/api/user/" $ do
  -- TODO auth token
  token <- xtractr
  result <- liftIO $ authorizedUser pool token
  case result of
    Just user -> json user
    Nothing -> raiseStatus status500 "disaster strikes"
