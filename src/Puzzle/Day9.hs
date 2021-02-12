{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Puzzle.Day9 where

import qualified Data.List as L
import Data.Vector ((!))
import qualified Data.Vector as V
import Import
import qualified RIO.HashSet as S
import qualified RIO.Text as T
import Prelude (read)

run :: Input -> RIO App ()
run (Input input) = do
  let xs = read . T.unpack <$> T.lines input
      result = L.head $ solve xs 25
  logInfo (fromString $ "Result " <> show result)

solve :: [Int64] -> Int -> [Int64]
solve xs pre =
  let vec = V.fromList xs
      isValid i = S.member (vec ! i) (preambles vec pre i)
      keepInvalid i = bool (Just $ vec ! i) Nothing (isValid i)
   in mapMaybe keepInvalid [pre .. (V.length vec - 1)]

preambles :: Vector Int64 -> Int -> Int -> S.HashSet Int64
preambles vec pre pos =
  let xs = V.slice (pos - pre) pre vec
   in S.fromList . catMaybes . V.toList $ do
        x <- xs
        y <- xs
        let r = if x == y then Nothing else Just (x + y)
        return r
