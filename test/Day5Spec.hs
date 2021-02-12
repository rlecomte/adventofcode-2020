{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day5Spec (spec) where

import Import
import Puzzle.Day5
import Test.Hspec

spec :: Spec
spec = do
  describe "find upper seat id" $ do
    it "compute seat id check" $
      computeSeatId [CUp, CDown, CDown, CDown, CUp, CUp, CDown, CUp, CUp, CUp] `shouldBe` SeatId 567
