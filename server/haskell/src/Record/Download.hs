module Record.Download (download) where

import Auth.JWT (authorizedUserId)
import DB (execute)
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import qualified Data.Vector as V
import qualified Hasql.Pool as HP
import qualified Hasql.Statement as S
import qualified Hasql.TH as TH
import Network.HTTP.Types (status200)
import Record.Go (Color (Black, White))
import SGF (Game (..), toSGF)
import Web.Scotty
  ( ScottyM,
    get,
    param,
    setHeader,
    status,
    text,
  )

type RecordRow = (Int64, T.Text, T.Text, T.Text, T.Text, Int64, Double, T.Text, T.Text, UTCTime)

selectRecord :: S.Statement (Int64, Int64) RecordRow
selectRecord =
  [TH.singletonStatement|
    select
    board_size :: int8, name :: text, black_player :: text, white_player :: text, comment :: text, handicap :: int8, komi :: float8, ruleset :: text, winner :: text, created :: timestamptz
    from record_record
    where
    owner_id = $1 :: int8 and id = $2 :: int8
  |]

type MoveRow = (Maybe Int64, T.Text)

selectMoves :: S.Statement Int64 (V.Vector MoveRow)
selectMoves =
  [TH.vectorStatement|
    select
    position :: int8?, color :: text
    from record_move
    where
    record_id = $1 :: int8
  |]

toColor :: T.Text -> Color
toColor color' = case color' of
  "B" -> Black
  "W" -> White
  _ -> White -- TODO????

download :: HP.Pool -> ScottyM ()
download pool = get "/api/records/:recordId/download/" $ do
  userId <- authorizedUserId
  recordId <- param "recordId"
  record <- execute pool selectRecord (userId, recordId)
  moves' <- execute pool selectMoves recordId
  let moves = [(fmap fromIntegral pos', toColor color') | (pos', color') <- V.toList moves']
      (size', name', blackPlayer, whitePlayer, comment', handicap, komi, _ruleset, winner', created) = record
      size = fromIntegral size'
      name = if name' == "" then Nothing else Just $ T.unpack name'
      comment = if comment' == "" then Nothing else Just $ T.unpack comment'
      dateString = formatTime defaultTimeLocale "_%Y_%m_%d.sgf" created
      filename = case name of
        Just name'' -> name'' ++ dateString
        Nothing -> T.unpack blackPlayer ++ "_vs_" ++ T.unpack whitePlayer ++ dateString
      winner = case winner' of
        "B" -> Just Black
        "W" -> Just White
        _ -> Nothing
      game =
        Game
          { name,
            comment,
            handicap = fromIntegral handicap,
            komi,
            blackPlayer = T.unpack blackPlayer,
            whitePlayer = T.unpack whitePlayer,
            winner,
            size = size,
            moves
          }
  status status200
  setHeader "Content-Type" "application/x-go-sgf"
  setHeader "Content-Disposition" $ TL.pack $ "attachment; filename=\"" ++ filename ++ "\""
  text $ TL.pack $ toSGF game
