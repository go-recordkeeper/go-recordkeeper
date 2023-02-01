module SGF (Game (..), toSGF) where

import Record.Go (Color (Black, White), Move, Pos, toCoord')

data Game = Game
  { name :: Maybe String,
    comment :: Maybe String,
    handicap :: Int,
    komi :: Double,
    blackPlayer :: String,
    whitePlayer :: String,
    winner :: Maybe Color,
    size :: Int,
    moves :: [Move]
  }

alphabet :: String
alphabet = "abcdefghijklmnopqrst"

formatPos :: Int -> Maybe Pos -> String
formatPos _ Nothing = "tt"
formatPos size (Just pos) = (alphabet !! x) : [alphabet !! y]
  where
    (x, y) = toCoord' size pos

formatMove :: Int -> Move -> String
formatMove size (pos, Black) = "B[" ++ formatPos size pos ++ "]"
formatMove size (pos, White) = "W[" ++ formatPos size pos ++ "]"

toSGF :: Game -> String
toSGF Game {name, comment, handicap, komi, blackPlayer, whitePlayer, winner, size, moves} =
  "(;FF[4]CA[UTF-8]"
    ++ ( case comment of
           Just comment' -> "GC[" ++ comment' ++ "]"
           Nothing -> ""
       )
    ++ "GM[1]"
    ++ ( case name of
           Just name' -> "GN[" ++ name' ++ "]"
           Nothing -> ""
       )
    ++ ("HA[" ++ show handicap ++ "]")
    ++ ("KM[" ++ show komi ++ "]")
    ++ ("PB[" ++ blackPlayer ++ "]")
    ++ ("PW[" ++ whitePlayer ++ "]")
    ++ ( "RE["
           ++ ( case winner of
                  Nothing -> "Void"
                  Just Black -> "B+R"
                  Just White -> "W+R"
              )
           ++ "]"
       )
    ++ ("SZ[" ++ show size ++ "]")
    ++ foldl
      (\a b -> a ++ ";" ++ b)
      ""
      (map (formatMove size) moves)
    ++ ")"
