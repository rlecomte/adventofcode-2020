{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run
  ( run,
  )
where

import Import
import qualified Puzzle.Day10 as Day10
import qualified Puzzle.Day11 as Day11
import qualified Puzzle.Day12 as Day12
import qualified Puzzle.Day13 as Day13
import qualified Puzzle.Day14 as Day14
import qualified Puzzle.Day3 as Day3
import qualified Puzzle.Day4 as Day4
import qualified Puzzle.Day5 as Day5
import qualified Puzzle.Day6 as Day6
import qualified Puzzle.Day7 as Day7
import qualified Puzzle.Day8 as Day8
import qualified Puzzle.Day9 as Day9
import RIO.FilePath ((</>))

run :: RIO App ()
run = loadInput >>= execute

loadInput :: RIO App Input
loadInput = do
  opts <- appOptions <$> ask
  let (Day day) = optionsDay opts
      inputPath = optionsDirEntry opts
  Input <$> readFileUtf8 (inputPath </> ("day" <> show day))

execute :: Input -> RIO App ()
execute input = do
  opts <- appOptions <$> ask
  let (Day day) = optionsDay opts
  case day of
    3 -> Day3.run input
    4 -> Day4.run input
    5 -> Day5.run input
    6 -> Day6.run input
    7 -> Day7.run input
    8 -> Day8.run input
    9 -> Day9.run input
    10 -> Day10.run input
    11 -> Day11.run input
    12 -> Day12.run input
    13 -> Day13.run input
    14 -> Day14.run input
    _ -> pure ()
