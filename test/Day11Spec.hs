{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day11Spec
  ( spec,
  )
where

import Import
import NeatInterpolation
import Puzzle.Day11
import Test.Hspec

spec :: Spec
spec = do
  describe "count occupied seat" $ do
    it "basic check" $
      solve
        ( Input
            [text|
L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL
            |]
        )
        `shouldBe` 37
