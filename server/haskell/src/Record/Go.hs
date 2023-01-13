{-# HLINT ignore "Use tuple-section" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Record.Go () where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (isNothing)
import Data.Set (Set)
import qualified Data.Set as Set

data Color = White | Black deriving (Eq)

type Pos = Int

type Coord = (Int, Int)

toCoord :: Int -> Pos -> Coord
toCoord boardSize pos = (mod pos boardSize, div pos boardSize)

toPos :: Int -> Coord -> Pos
toPos boardSize (x, y) = (y * boardSize) + x

adjacents :: Int -> Coord -> [Coord]
adjacents boardSize (x, y) =
  [(x', y') | (x', y') <- [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)], 0 <= x', x' < boardSize, 0 <= y', y' < boardSize]

-- Map the Pos to the color of the stone occupying that space
type Board = IntMap Color

buildGroup' :: Int -> Board -> Color -> Set Int -> Set Int -> [Coord] -> (Set Int, Set Int)
buildGroup' boardSize board color group liberties coordsToCheck
  | null coordsToCheck = (group, liberties)
  | Set.member pos group = buildGroup' boardSize board color group liberties $ tail coordsToCheck
  | otherwise = buildGroup' boardSize board color group' liberties' coordsToCheck'
  where
    coord = head coordsToCheck
    pos = toPos boardSize coord
    stone = board IntMap.!? toPos boardSize coord
    (group', coordsToCheck') =
      if stone == Just color
        then (Set.insert pos group, tail coordsToCheck ++ adjacents boardSize coord)
        else (group, tail coordsToCheck)
    liberties' =
      if isNothing stone
        then Set.insert pos liberties
        else liberties

buildGroup :: Int -> Board -> Coord -> (Set Int, Set Int)
buildGroup boardSize board coord =
  case intersection of
    Just color -> buildGroup' boardSize board color group liberties coordsToCheck
    Nothing -> (group, liberties)
  where
    intersection = board IntMap.!? toPos boardSize coord
    group = Set.empty
    liberties = Set.empty
    coordsToCheck = [coord]

attemptMurder :: Int -> Board -> Coord -> Board
attemptMurder boardSize board coord =
  if Set.null liberties
    then IntMap.filterWithKey (\pos _ -> not $ Set.member pos group) board
    else board
  where
    (group, liberties) = buildGroup boardSize board coord

placeStone :: Int -> Board -> Pos -> Color -> Board
placeStone boardSize board pos color =
  -- TODO check if space is already occupied
  -- TODO check if move is suicidal
  foldl (attemptMurder boardSize) board' $ adjacents boardSize coord
  where
    board' = IntMap.insert pos color board
    coord = toCoord boardSize pos

--
--
--
--
--
--
--
--
-- TODO figure out all this monad ****
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
