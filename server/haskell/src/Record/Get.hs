module Record.Get (getRecord) where

import Auth.JWT (authorizedUserId)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Int (Int64)
import qualified Data.IntMap.Strict as IntMap
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time (UTCTime)
import qualified Data.Vector as V
import qualified Hasql.Pool as HP
import qualified Hasql.Session as HS
import qualified Hasql.Statement as S
import qualified Hasql.TH as TH
import Network.HTTP.Types (status200, status404, status500)
import Record.Go (Color (Black, White), identifyCaptures, runBoardA, toCoord')
import Web.Scotty
  ( ScottyM,
    get,
    json,
    param,
    raiseStatus,
    status,
  )

data Capture = Capture {x :: Int, y :: Int} deriving (Show)

$(deriveJSON defaultOptions ''Capture)

data Move = Move {position :: Int, color :: String, captures :: [Capture]} deriving (Show)

$(deriveJSON defaultOptions ''Move)

data Stone = Stone {x :: Int, y :: Int, color :: String} deriving (Show)

$(deriveJSON defaultOptions ''Stone)

data GetResponse = GetResponse
  { id :: Int,
    owner :: Int,
    board_size :: Int,
    created :: UTCTime,
    name :: String,
    black_player :: String,
    white_player :: String,
    comment :: String,
    handicap :: Int,
    komi :: Double,
    ruleset :: String,
    winner :: String,
    -- TODO moves
    moves :: [Move],
    stones :: [Stone]
  }
  deriving (Show)

$(deriveJSON defaultOptions ''GetResponse)

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

type MoveRow = (Int64, T.Text)

selectMoves :: S.Statement Int64 (V.Vector MoveRow)
selectMoves =
  [TH.vectorStatement|
    select
    position :: int8, color :: text
    from record_move
    where
    record_id = $1 :: int8
  |]

fromColor :: Color -> String
fromColor color = case color of
  Black -> "B"
  White -> "W"

toResponse :: Int64 -> Int64 -> RecordRow -> [((Int, Color), Set Int)] -> [((Int, Int), Color)] -> GetResponse
toResponse userId recordId (board_size, name, black_player, white_player, comment, handicap, komi, ruleset, winner, created) moves stones =
  GetResponse
    { id = fromIntegral recordId,
      owner = fromIntegral userId,
      board_size = fromIntegral board_size,
      created,
      name = T.unpack name,
      black_player = T.unpack black_player,
      white_player = T.unpack white_player,
      comment = T.unpack comment,
      handicap = fromIntegral handicap,
      komi,
      ruleset = T.unpack ruleset,
      winner = T.unpack winner,
      moves =
        [ Move
            { position,
              color = fromColor color,
              captures = [Capture {x, y} | (x, y) <- map (toCoord' $ fromIntegral board_size) $ Set.toAscList captures]
            }
          | ((position, color), captures) <- moves
        ],
      stones =
        [ Stone
            { x,
              y,
              color = if color == Black then "B" else "W"
            }
          | ((x, y), color) <- stones
        ]
    }

getRecord :: HP.Pool -> ScottyM ()
getRecord pool = get "/api/records/:recordId/" $ do
  userId <- authorizedUserId
  recordId <- param "recordId"
  recordSelect <- liftIO $ HP.use pool $ HS.statement (userId, recordId) selectRecord
  movesSelect <- liftIO $ HP.use pool $ HS.statement recordId selectMoves
  case (recordSelect, movesSelect) of
    (Right record, Right moves') -> do
      let moves = V.toList moves'
      -- TODO determine captures for each move
      -- TODO desgostang
      let (size', _, _, _, _, _, _, _, _, _) = record
      let size = fromIntegral size'
      -- TODO effective error checking for invalid move strings
      let movesToPlay = [(fromIntegral position, if color == "B" then Black else White) | (position, color) <- moves]
      case runBoardA size $ identifyCaptures movesToPlay of
        Left _ -> raiseStatus status500 "Invalid game record."
        Right (movesAndCaptures, board) -> do
          -- TODO order stones stably in integration tests
          let stones = [(toCoord' size pos, color) | (pos, color) <- IntMap.toDescList board]
          status status200
          json $ toResponse userId recordId record movesAndCaptures stones
    (Left (HP.SessionError (HS.QueryError _ _ (HS.ResultError (HS.UnexpectedAmountOfRows 0)))), _) -> do
      raiseStatus status404 "Does not exist."
    (x, y) -> do
      liftIO $ putStrLn "Oh ho"
      liftIO $ print x
      liftIO $ print y
      raiseStatus status500 $ TL.pack $ show y
