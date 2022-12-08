module Auth.Get (getUser) where

import Auth.User
import Control.Monad.IO.Class
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int64)
import qualified Data.Text.Lazy as DT
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Pool as HP
import qualified Hasql.Session as HS
import qualified Hasql.Statement as S
import Network.HTTP.Types.Status (status200, status401, status500)
import Web.Scotty

data GetResponse = GetResponse
  { id :: Int,
    username :: String,
    email :: String
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''GetResponse)

xtractr :: ActionM DT.Text
xtractr = do
  headerValue <- header "Authorization"
  let token = DT.stripPrefix "Bearer " =<< headerValue
  maybe (raiseStatus status401 "Not Authorized") pure token

sumStatement :: S.Statement (Int64, Int64) Int64
sumStatement = S.Statement sql encoder decoder True
  where
    sql = "select $1 + $2"
    encoder =
      (fst >$< Encoders.param (Encoders.nonNullable Encoders.int8))
        <> (snd >$< Encoders.param (Encoders.nonNullable Encoders.int8))
    decoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8))

getUser :: HP.Pool -> ScottyM ()
getUser pool = get "/api/user/" $ do
  -- TODO auth token
  foo <- xtractr
  let sess = HS.statement (5, 6) sumStatement
  result <- liftIO $ HP.use pool sess
  case result of
    Right thing -> json (User {id' = fromIntegral thing, name = DT.unpack foo, email = "bar", password = "pass"})
    Left _ -> raiseStatus status500 "disaster strikes"
