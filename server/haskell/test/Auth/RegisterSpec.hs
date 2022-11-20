{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Auth.RegisterSpec (spec) where

import Auth.Register
import Servant
import Servant.Client (ClientError, ClientM, HasClient (Client))
import Test.Hspec

-- lmao this is the original test implementation

-- spec :: SpecWith ClientEnv
-- spec = do
--   let proxy = (Proxy :: Proxy ("api" :> RegisterAPI))
--   let registerClient = client proxy
--   describe "unhelped" $ do
--     it "prolly don't work" $ \clientEnv -> do
--       result <- runClientM (registerClient RegisterRequest {username = "foo", email = "bar", password = "baz"}) clientEnv
--       result `shouldBe` (Right $ RegisterResponse {id = 1, username = "foo", email = "bar"})

-- lmao this should be in its own file

type family ClientRequestTypeFamily a

type instance ClientRequestTypeFamily (req -> ClientM resp) = req

type ClientRequestType api = ClientRequestTypeFamily (Client ClientM api)

type family ClientResponseTypeFamily a

type instance ClientResponseTypeFamily (req -> ClientM resp) = resp

type ClientResponseType api = ClientResponseTypeFamily (Client ClientM api)

type ClientGenerator api = (Proxy api -> ClientRequestType api -> IO (Either ClientError (ClientResponseType api)))

-- lmao this is the test

spec :: SpecWith (ClientGenerator ("api" :> RegisterAPI))
spec = describe "Register" $ do
  let proxy = Proxy :: Proxy ("api" :> RegisterAPI)
  it "registers" $ \client -> do
    result <- client proxy RegisterRequest {username = "foo", email = "bar", password = "baz"}
    result `shouldBe` (Right $ RegisterResponse {id = 1, username = "foo", email = "bar"})
