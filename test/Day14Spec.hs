{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day14Spec
  ( spec,
  )
where

import Import
import NeatInterpolation
import Puzzle.Day14
import Test.Hspec

spec :: Spec
spec = do
  describe "Docking data" $ do
    it "basic check" $
      solve
        ( Input
            [text|
mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0
            |]
        )
        `shouldBe` Right 165
