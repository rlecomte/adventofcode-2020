{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Import
import Options.Applicative.Simple
import qualified Paths_adventofcode
import RIO.Process
import Run

main :: IO ()
main = do
  (options, ()) <-
    simpleOptions
      $(simpleVersion Paths_adventofcode.version)
      "Advent of code 2020"
      "Each day, a new puzzle"
      ( Options
          <$> ( Day
                  <$> option
                    auto
                    ( long "day"
                        <> short 'd'
                        <> help "day puzzle to execute."
                    )
              )
          <*> option
            str
            ( long "input-dir"
                <> short 'i'
                <> help "puzzle input directory."
            )
      )
      empty
  lo <- logOptionsHandle stderr True
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app =
          App
            { appLogFunc = lf,
              appProcessContext = pc,
              appOptions = options
            }
     in runRIO app run
