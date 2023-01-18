module Record.GoSpec (spec) where

import qualified Data.IntMap.Strict as IntMap
import Record.Go (BoardA, Color (Black, White), GoError (OutOfBounds), adjacents, boardSize, placeStone, playStones, runBoardA, toCoord, toPos)
import Test.Hspec

withBoardSizes :: BoardA Expectation -> Expectation
withBoardSizes test = mapM_ (\size -> noError $ runBoardA size test) [9, 13, 19]

withStones :: [(Color, Int, Int)] -> BoardA Expectation -> Expectation
withStones stones = withBoardSizes

noError :: Either GoError a -> a
noError (Left e) = error $ show e
noError (Right a') = a'

expectError :: GoError -> Either GoError a -> Expectation
expectError expected (Left actual) = expected `shouldBe` actual
expectError _ (Right _) = error "Test did not fail"

spec :: Spec
spec = describe "Record.Go" $ do
  it "converts pos to coord to pos" $ withBoardSizes $ do
    size <- boardSize
    sequence_
      <$> sequence
        [ do
            coord <- toCoord pos
            pos' <- toPos coord
            return $ pos' `shouldBe` pos
          | pos <- [0 .. (size * size) - 1]
        ]

  it "converts coord to pos to coord" $ withBoardSizes $ do
    size <- boardSize
    sequence_
      <$> sequence
        [ do
            pos <- toPos (x, y)
            coord' <- toCoord pos
            return $ coord' `shouldBe` (x, y)
          | x <- [0 .. size - 1],
            y <- [0 .. size - 1]
        ]

  it "calculates correct adjacents for corners" $ withBoardSizes $ do
    adjs <- adjacents (0, 0)
    -- TODO other corners
    return $ adjs `shouldBe` [(1, 0), (0, 1)]

  it "calculates correct adjacents for edges" $ withBoardSizes $ do
    topAdjs <- adjacents (1, 0)
    leftAdjs <- adjacents (0, 1)
    -- TODO other sides
    return $
      sequence_
        [ topAdjs `shouldBe` [(0, 0), (2, 0), (1, 1)],
          leftAdjs `shouldBe` [(1, 1), (0, 0), (0, 2)]
        ]

  it "calculates correct adjacents in the middle" $ withBoardSizes $ do
    adjs <- adjacents (1, 1)
    return $ adjs `shouldBe` [(0, 1), (2, 1), (1, 0), (1, 2)]

  it "places a stone" $ withBoardSizes $ do
    let board = IntMap.empty
    board <- placeStone board (0, Black)
    return $
      sequence_
        [ IntMap.size board `shouldBe` 1,
          (board IntMap.!? 0) `shouldBe` Just Black
        ]

  it "cannot place a stone at -1" $ expectError (OutOfBounds (-1)) $ runBoardA 9 $ do
    let board = IntMap.empty
    placeStone board (-1, Black)

  it "cannot place a stone beyond the board" $ expectError (OutOfBounds 81) $ runBoardA 9 $ do
    let board = IntMap.empty
    placeStone board (81, Black)

  it "plays two stones" $ withBoardSizes $ do
    board <- playStones [(0, Black), (1, White)]
    return $
      sequence_
        [ IntMap.size board `shouldBe` 2,
          (board IntMap.!? 0) `shouldBe` Just Black,
          (board IntMap.!? 1) `shouldBe` Just White
        ]

  it "captures a stone" $ noError $ runBoardA 9 $ do
    board <- playStones [(0, Black), (1, White), (10, Black), (9, White)]
    return $
      sequence_
        [ IntMap.size board `shouldBe` 3,
          (board IntMap.!? 0) `shouldBe` Nothing,
          (board IntMap.!? 1) `shouldBe` Just White,
          (board IntMap.!? 9) `shouldBe` Just White,
          (board IntMap.!? 10) `shouldBe` Just Black
        ]

  it "captures a group" $ noError $ runBoardA 9 $ do
    board <- playStones [(0, Black), (9, White), (1, Black), (10, White), (2, Black), (11, White), (12, Black), (3, White)]
    return $
      sequence_
        [ IntMap.size board `shouldBe` 5,
          (board IntMap.!? 0) `shouldBe` Nothing,
          (board IntMap.!? 1) `shouldBe` Nothing,
          (board IntMap.!? 2) `shouldBe` Nothing,
          (board IntMap.!? 3) `shouldBe` Just White,
          (board IntMap.!? 9) `shouldBe` Just White,
          (board IntMap.!? 10) `shouldBe` Just White,
          (board IntMap.!? 11) `shouldBe` Just White,
          (board IntMap.!? 12) `shouldBe` Just Black
        ]
