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
      let registerClient = client (Proxy :: Proxy ("api" :> RegisterAPI))
      baseUrl <- runIO $ parseBaseUrl "http://localhost"
      manager <- runIO $ newManager defaultManagerSettings
      let clientEnv port = mkClientEnv manager (baseUrl {baseUrlPort = port})
      describe "polite tests" $ do
        it "should equal" $ \port -> do
          result <- runClientM (registerClient RegisterRequest {username = "foo", email = "bar", password = "baz"}) (clientEnv port)
          result `shouldBe` (Right $ RegisterResponse {id = 1, username = "foo", email = "bar"})
