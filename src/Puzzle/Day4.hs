{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Puzzle.Day4 where

import qualified Data.List as L
import Data.Monoid (All (..))
import Import
import qualified RIO.Text as T

run :: Input -> RIO App ()
run input = do
  let result = solve input
  logInfo (fromString $ "Number of valid passport : " <> show result)

solve :: Input -> Int
solve (Input input) = L.length . filter (isValid . T.unlines) . L.unfoldr extractUnit $ T.lines input
  where
    isBlankLine = T.null . T.strip

    dropHeader = L.dropWhile isBlankLine

    extractUnit l =
      let l' = dropHeader l
       in bool (Just $ L.break isBlankLine l') Nothing (L.null l')

    isValid fields = getAll $ foldMap (\f -> All $ T.isInfixOf f fields) ["ecl", "pid", "eyr", "hcl", "byr", "iyr", "hgt"]
