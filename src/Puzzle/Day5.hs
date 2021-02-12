{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Puzzle.Day5 where

import qualified Data.List as L
import Data.Semigroup (Max (..))
import Import
import qualified RIO.Text as T

data Cursor = CDown | CUp

newtype SeatId = SeatId Int deriving (Show, Eq, Ord, Bounded)

run :: Input -> RIO App ()
run input = do
  let result = solve input
  logInfo (fromString $ "Upper seat id : " <> show result)

solve :: Input -> SeatId
solve (Input input) = getMax $ foldMap (Max . computeSeatId . map toCursor . T.unpack) $ T.lines input
  where
    toCursor 'B' = CUp
    toCursor 'F' = CDown
    toCursor 'R' = CUp
    toCursor 'L' = CDown
    toCursor _ = CDown

computeSeatId :: [Cursor] -> SeatId
computeSeatId xs = SeatId $ row * 8 + col
  where
    row = L.foldl' reduceRange 0 $ L.zip [64, 32, 16, 8, 4, 2, 1] $ L.take 7 xs

    col = L.foldl' reduceRange 0 $ L.zip [4, 2, 1] $ L.drop 7 xs

    reduceRange :: Int -> (Int, Cursor) -> Int
    reduceRange a (x, c) =
      case c of
        CDown -> a
        CUp -> a + x
