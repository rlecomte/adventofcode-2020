{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Puzzle.Day3 where

import Data.List ((!!))
import qualified Data.List as L
import Import
import qualified RIO.Text as T

run :: Input -> RIO App ()
run input = do
  let result = solve input
  logInfo (fromString $ "Number of tree on path : " <> show result)

solve :: Input -> Int
solve (Input input) =
  let puzzleMap = zip [0 ..] $ fmap (L.cycle . T.unpack) (T.lines input)
   in L.length $ filter (== '#') $ fmap (\(i, l) -> l !! (i * 3 :: Int)) puzzleMap
