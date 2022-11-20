---
title: "Ditching Servant"
date: 2022-11-19T19:29:16-05:00
type: post
slug: "12-ditching-servant"
tags:
  - haskell
---

As promised, I have gained some ground on testing. [servant-quickcheck](https://github.com/haskell-servant/servant-quickcheck) is apparently dead and therefore not included in Stackage, which was obnoxious because it was still listed in the Servant tutorial. Still, I persevered and got a rudimentary test for the register endpoint working:

```haskell
spec :: SpecWith ClientEnv
spec = describe "Register" $ do
  let proxy = (Proxy :: Proxy ("api" :> RegisterAPI))
  let registerClient = client proxy
  it "registers" $ \clientEnv -> do
    result <- runClientM (registerClient RegisterRequest {username = "foo", email = "bar", password = "baz"}) clientEnv
    result `shouldBe` (Right $ RegisterResponse {id = 1, username = "foo", email = "bar"})
```

However, I wasn't impressed with the `runClientM (...) clientEnv` bit. `runClientM` is the thing that actually calls the client, and it requires both the request information (`RegisterRequest {...}`) and the `clientEnv`, which is basically just context. Thing is, the `clientEnv` doesn't matter at all to the test, and the test doesn't need to call `runClientM` directly. I was already passing in the `clientEnv` to the test function, so I thought it would be easy enough simply wrap all the `runClientM`/`clientEnv` stuff into a simple function and pass that to the test function instead. That would eliminate some boilerplate from every test, and also avoid importing `Servant.Client` in the test file at all. This is the test I wanted to write:

```haskell
spec :: SpecWith (ClientGenerator ("api" :> RegisterAPI))
spec = describe "Register" $ do
  let proxy = Proxy :: Proxy ("api" :> RegisterAPI)
  it "registers" $ \testClient -> do
    result <- testClient proxy RegisterRequest {username = "foo", email = "bar", password = "baz"}
    result `shouldBe` (Right $ RegisterResponse {id = 1, username = "foo", email = "bar"})
```

Sadly, this is not easy. It is not easy at all.

The problem is that the `testClient` (of type `ClientGenerator`) needs to take the API type (in this case, `"api" :> RegisterAPI`) and return a function of type `request -> IO(Either ClientError response)`. I spent two days banging my head against the problem until a friend showed me the [~](https://hackage.haskell.org/package/ghc-prim-0.6.1/docs/src/GHC.Types.html#~) constraint operator. Applying this little tidbit and a healthy sprinkling of type family magic as well, this is the resulting type definition for `ClientGenerator`:

```haskell
type family ClientRequestTypeFamily a

type instance ClientRequestTypeFamily (req -> ClientM resp) = req

type ClientRequestType api = ClientRequestTypeFamily (Client ClientM api)

type family ClientResponseTypeFamily a

type instance ClientResponseTypeFamily (req -> ClientM resp) = resp

type ClientResponseType api = ClientResponseTypeFamily (Client ClientM api)

type ClientGenerator api = (Proxy api -> ClientRequestType api -> IO (Either ClientError (ClientResponseType api)))
```

Quite frankly, this is awful. This is all necessary because Servant dumps all of the API descriptors into the type system. Wrestling with the type system is the only way to get any API information out again.

The test does finally work and my little bit of boilerplate was eliminated, but quite frankly I am traumatized.

I'm going to switch to [Scotty](https://github.com/scotty-web/scotty) instead of Servant. It seems to be just as concise and hopefully not as reliant on the type system.