module Record.GoSpec (spec) where

import qualified Data.IntMap.Strict as IntMap
import Record.Go (BoardA, Color (Black, White), adjacents, boardSize, placeStone, runBoardA, toCoord, toPos)
import Test.Hspec

withBoardSizes :: BoardA Expectation -> Expectation
withBoardSizes test = mapM_ (`runBoardA` test) [9, 13, 19]

withStones :: [(Color, Int, Int)] -> BoardA Expectation -> Expectation
withStones stones = withBoardSizes

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

  -- it "builds a one stone group" $ withBoardSizes $ do
  --   return

  it "places a stone" $ withBoardSizes $ do
    b <- placeStone 0 Black
    return $
      sequence_
        [ IntMap.size b `shouldBe` 1,
          (b IntMap.!? 0) `shouldBe` Just Black
        ]
