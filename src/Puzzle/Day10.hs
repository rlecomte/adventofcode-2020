{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Puzzle.Day10 where

import qualified Data.List as L
import Import
import qualified RIO.Text as T
import Prelude (read)

run :: Input -> RIO App ()
run (Input input) = do
  let xs = read . T.unpack <$> T.lines input
      result = solve xs
  logInfo (fromString $ "Result " <> show result)

solve :: [Int64] -> Int
solve xs =
  let sorted = L.sort xs
      (a, b) = L.partition (< 3) $ fmap (uncurry (-)) $ zip sorted (0 : sorted)
   in L.length a * (L.length b + 1)
