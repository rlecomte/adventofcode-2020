{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day9Spec (spec) where

import Import
import Puzzle.Day9
import Test.Hspec

spec :: Spec
spec = do
  describe "count invalid encoding" $ do
    it "basic check" $
      solve
        [ 35,
          20,
          15,
          25,
          47,
          40,
          62,
          55,
          65,
          95,
          102,
          117,
          150,
          182,
          127,
          219,
          299,
          277,
          309,
          576
        ]
        5
        `shouldBe` [127]
