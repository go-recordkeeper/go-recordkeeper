{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Auth.JWT (generateJWK, authorizedUserId) where

import Control.Lens (view, (^?))
import Control.Lens.Setter (set)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Crypto.JOSE (JWK, decodeCompact)
import Crypto.JOSE.JWK (fromOctets)
import Crypto.JWT (AsError, ClaimsSet, JWTError, SignedJWT, allowedSkew, claimSub, defaultJWTValidationSettings, string, verifyClaims)
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Network.HTTP.Types.Status (status403, status500)
import System.Environment (getEnv, lookupEnv)
import Web.Scotty
  ( ActionM,
    header,
    raiseStatus,
  )
import Web.Scotty.Internal.Types (ActionError)

generateJWK :: IO JWK
generateJWK = do
  dev <- lookupEnv "GOBAN_DEVELOPMENT"
  secretKey <- case dev of
    Just _ -> pure "django-insecure-(@ppnpk$wx_z%2^#^0sext&+%b58=%e^!_u_*yd2p#d2&9)9cj"
    Nothing -> getEnv "GOBAN_SECRET_KEY"
  let jwk = fromOctets $ T.encodeUtf8 $ T.pack secretKey
  pure jwk

verifyJWT :: JWK -> SignedJWT -> IO (Either JWTError ClaimsSet)
verifyJWT jwk jwt = runExceptT $ do
  let config = set allowedSkew 10 $ defaultJWTValidationSettings (== "go-recordkeeper")
  verifyClaims config jwk jwt

-- TODO figure out exactly what's up here
instance AsError (ActionError a)

getAuthToken :: ActionM TL.Text
getAuthToken = do
  headerValue <- header "Authorization"
  let token = TL.stripPrefix "Bearer " =<< headerValue
  maybe (raiseStatus status403 "Not Authorized") pure token

authorizedUserId :: ActionM Int64
authorizedUserId = do
  token <- getAuthToken
  jwk <- liftIO generateJWK
  jwt <- decodeCompact $ TL.encodeUtf8 token
  result <- liftIO $ verifyJWT jwk jwt
  case result of
    Right claims -> do
      case view claimSub claims of
        Just id' -> do
          case id' ^? string of
            Just id'' -> return $ read $ T.unpack id''
            Nothing -> raiseStatus status500 "sub is a URL?????"
        Nothing -> raiseStatus status500 "claim has no sub???"
    Left _err -> raiseStatus status500 "disaster parsin that token"
