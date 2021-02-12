{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day6Spec (spec) where

import Import
import Puzzle.Day6
import Test.Hspec

spec :: Spec
spec = do
  describe "count answer for a group" $ do
    it "basic check" $
      groupCount ["abc"] `shouldBe` 3
