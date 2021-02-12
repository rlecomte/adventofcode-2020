{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day4Spec (spec) where

import Data.FileEmbed (embedStringFile)
import Import
import Puzzle.Day4
import Test.Hspec

spec :: Spec
spec = do
  describe "count valid passport" $ do
    it "basic check" $
      solve (Input testFile) `shouldBe` 2

testFile :: IsString a => a
testFile = $(embedStringFile "input/test4")
