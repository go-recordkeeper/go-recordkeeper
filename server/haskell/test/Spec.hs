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
    Auth.RegisterSpec.spec jwk