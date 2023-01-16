{-# LANGUAGE DeriveFunctor #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Record.Go
  ( BoardS,
    toCoord',
    toPos',
    adjacents,
  )
where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (isNothing)
import Data.Set (Set)
import qualified Data.Set as Set

data Color = White | Black deriving (Eq)

type Pos = Int

type Coord = (Int, Int)

-- Map the Pos to the color of the stone occupying that space
type Board = IntMap Color

-- Several functions need the board and it's size, so wrap that into a single type
newtype BoardS = BoardS {boardState :: (Int, Board)}

mapBoard :: (Board -> Board) -> BoardS -> BoardS
mapBoard f (BoardS (size, b)) = BoardS (size, f b)

boardSize :: BoardS -> Int
boardSize (BoardS (size, _)) = size

board :: BoardS -> Board
board (BoardS (_, board')) = board'

toCoord' :: Int -> Pos -> Coord
toCoord' size pos = (mod pos size, div pos size)

toCoord :: BoardS -> Pos -> Coord
toCoord state = toCoord' $ boardSize state

toPos' :: Int -> Coord -> Pos
toPos' size (x, y) = (y * size) + x

toPos :: BoardS -> Coord -> Pos
toPos state = toPos' $ boardSize state

adjacents :: BoardS -> Coord -> [Coord]
adjacents state (x, y) =
  [(x', y') | (x', y') <- [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)], 0 <= x', x' < boardSize state, 0 <= y', y' < boardSize state]

buildGroup' :: BoardS -> Color -> Set Int -> Set Int -> [Coord] -> (Set Int, Set Int)
buildGroup' state color group liberties coordsToCheck
  | null coordsToCheck = (group, liberties)
  | Set.member pos group = buildGroup' state color group liberties $ tail coordsToCheck
  | otherwise = buildGroup' state color group' liberties' coordsToCheck'
  where
    coord = head coordsToCheck
    pos = toPos state coord
    stone = board state IntMap.!? toPos state coord
    (group', coordsToCheck') =
      if stone == Just color
        then (Set.insert pos group, tail coordsToCheck ++ adjacents state coord)
        else (group, tail coordsToCheck)
    liberties' =
      if isNothing stone
        then Set.insert pos liberties
        else liberties

buildGroup :: BoardS -> Coord -> (Set Int, Set Int)
buildGroup state coord =
  case intersection of
    Just color -> buildGroup' state color group liberties coordsToCheck
    Nothing -> (group, liberties)
  where
    intersection = board state IntMap.!? toPos state coord
    group = Set.empty
    liberties = Set.empty
    coordsToCheck = [coord]

attemptMurder :: BoardS -> Coord -> BoardS
attemptMurder state coord =
  if Set.null liberties
    then mapBoard (IntMap.filterWithKey (\pos _ -> not $ Set.member pos group)) state
    else state
  where
    (group, liberties) = buildGroup state coord

placeStone :: BoardS -> Pos -> Color -> BoardS
placeStone state pos color =
  -- TODO check if space is already occupied
  -- TODO check if move is suicidal
  foldl attemptMurder addedStone $ adjacents state coord
  where
    addedStone = mapBoard (IntMap.insert pos color) state
    coord = toCoord state pos

--
--
--
--
--
--
--
-- TODO figure out all this monad ****
newtype BoardA a = BoardA {runBoardAction :: a} deriving (Functor)

newtype BoardM s a = BoardM {runBoardM :: Int -> s -> (a, s, Int)}

instance Monad (BoardM s) where
  return x = BoardM $ \boardSize s -> (x, s, boardSize)
  m >>= f = BoardM $ \boardSize s ->
    let (x, s', _) = runBoardM m boardSize s
        (y, s'', _) = runBoardM (f x) boardSize s'
     in (y, s'', boardSize)

instance Applicative (BoardM s) where
  pure = return

  -- TODO express this in terms of >>=
  f <*> a = BoardM $ \boardSize s ->
    let (f', s', _) = runBoardM f boardSize s
        (a', s'', _) = runBoardM a boardSize s'
        b = f' a'
     in (b, s'', boardSize)

instance Functor (BoardM s) where
  fmap f a = f <$> a

-- foo :: Move -> BoardM Board Bool
-- foo move = do
--   return True

doIt :: Int -> BoardM Board a -> Board
doIt boardSize m =
  let (xx, yy, zz) = runBoardM m boardSize IntMap.empty
   in yy
