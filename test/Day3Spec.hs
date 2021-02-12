{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day3Spec (spec) where

import Import
import Puzzle.Day3
import Test.Hspec

spec :: Spec
spec = do
  describe "countTrees" $ do
    it "basic check" $
      solve (Input ".......\n...#...\n......#\n.......\n.....#.") `shouldBe` 3
