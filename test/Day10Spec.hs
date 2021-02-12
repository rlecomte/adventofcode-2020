{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day10Spec
  ( spec,
  )
where

import Import
import Puzzle.Day10
import Test.Hspec

spec :: Spec
spec = do
  describe "count number of jolt difference" $ do
    it "basic check" $
      solve
        [ 16,
          10,
          15,
          5,
          1,
          11,
          7,
          19,
          6,
          12,
          4
        ]
        `shouldBe` 35
