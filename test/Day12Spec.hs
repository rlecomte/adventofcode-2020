{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day12Spec
  ( spec,
  )
where

import Import
import Puzzle.Day12
import Test.Hspec

spec :: Spec
spec = do
  describe "compute manhattan distance" $ do
    it "basic check" $
      solve
        [ F 10,
          NN 3,
          F 7,
          R 90,
          F 11
        ]
        `shouldBe` 25
