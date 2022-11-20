{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Auth.RegisterSpec
import Control.Exception (evaluate)
import Crypto.JOSE (JWK)
import Lib
import Network.HTTP.Client hiding (Proxy)
import qualified Network.Wai.Handler.Warp as Warp
import Servant
import Servant.Auth.Server (generateKey)
import Servant.Client
import Test.Hspec
import Test.QuickCheck

-- withApp :: JWK -> (ClientEnv -> IO ()) -> IO ()
withApp :: (HasClient ClientM api, Client ClientM api ~ (t -> ClientM a)) => JWK -> ((Proxy api -> t -> IO (Either ClientError a)) -> IO a1) -> IO a1
withApp jwk spec' = Warp.testWithApplication (pure $ app jwk) $ \port -> do
  baseUrl <- parseBaseUrl "http://localhost"
  manager <- newManager defaultManagerSettings
  let clientEnv = mkClientEnv manager (baseUrl {baseUrlPort = port})
  spec' $ \proxy req -> runClientM (client proxy req) clientEnv

main :: IO ()
main = do
  jwk <- generateKey
  hspec $ do
    describe "Prelude.head" $ do
      it "returns the first element of a list" $ do
        head [23 ..] `shouldBe` (23 :: Int)
      it "returns the first element of an *arbitrary* list" $
        property $
          \x xs -> head (x : xs) == (x :: Int)
      it "throws an exception if used with an empty list" $ do
        evaluate (head []) `shouldThrow` anyException
    around (withApp jwk) $ do
      Auth.RegisterSpec.spec