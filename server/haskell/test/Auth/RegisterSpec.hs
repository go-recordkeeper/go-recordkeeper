module Auth.RegisterSpec (spec) where

import Auth.Register
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

withApp :: JWK -> (Warp.Port -> IO ()) -> IO ()
withApp jwk = Warp.testWithApplication (pure $ app jwk)

spec :: JWK -> Spec
spec jwk = around (withApp jwk) $ do
  let registerClient = client (Proxy :: Proxy ("api" :> RegisterAPI))
  baseUrl <- runIO $ parseBaseUrl "http://localhost"
  manager <- runIO $ newManager defaultManagerSettings
  let clientEnv port = mkClientEnv manager (baseUrl {baseUrlPort = port})
  describe "polite tests" $ do
    it "should equal" $ \port -> do
      result <- runClientM (registerClient RegisterRequest {username = "foo", email = "bar", password = "baz"}) (clientEnv port)
      result `shouldBe` (Right $ RegisterResponse {id = 1, username = "foo", email = "bar"})
