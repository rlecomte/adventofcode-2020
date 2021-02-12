{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Puzzle.Day6 where

import qualified Data.Bits as B
import qualified Data.List as L
import Data.Monoid (Sum (..))
import Import
import qualified RIO.Map as Map
import qualified RIO.Text as T

run :: Input -> RIO App ()
run input = do
  let result = solve input
  logInfo (fromString $ "Result " <> show result)

solve :: Input -> Int
solve (Input input) = getSum $ foldMap (Sum . groupCount . fmap T.unpack) $ L.unfoldr extractUnit $ T.lines input
  where
    isBlankLine = T.null . T.strip

    dropHeader = L.dropWhile isBlankLine

    extractUnit l =
      let l' = dropHeader l
       in bool (Just $ L.break isBlankLine l') Nothing (L.null l')

groupCount :: [String] -> Int
groupCount xs = count $ L.foldl' (B..|.) 0 $ fmap toBit =<< xs

toBit :: Char -> Int
toBit l = B.bit . fromMaybe 0 $ Map.lookup l letterIdx

count :: Int -> Int
count i = getSum $ foldMap (Sum . bool 0 1 . (/=) 0 . (B..&.) i) masks

letterIdx :: Map Char Int
letterIdx = Map.fromList $ zip ['a' .. 'z'] [0 ..]

masks :: [Int]
masks = fmap B.bit [0 .. 25]
