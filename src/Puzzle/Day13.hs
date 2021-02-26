{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Puzzle.Day13 where

import qualified Data.List as L
import qualified Data.Maybe as M
import Data.Semigroup (Arg (..), Min (..))
import Import
import qualified RIO.Text as T

data BusTs = BusTs {busBid :: Int, busTs :: Int} deriving (Show)

run :: Input -> RIO App ()
run input = do
  let result = solve input
  logInfo (fromString $ "Result " <> show result)

solve :: Input -> Int
solve (Input input) =
  let l = T.lines input
      departure = M.fromJust $ readMaybe $ T.unpack $ L.head l
      schedules = extractInputs departure $ L.head $ L.tail l
      (Arg _ b) = getMin $ M.fromJust $ foldMap (\x -> Just $ Min $ Arg (busTs x) x) schedules
   in busBid b * (busTs b - departure)

extractInputs :: Int -> Text -> [BusTs]
extractInputs departure = fmap (toBusTs . asBusId) . L.filter (not . (==) (T.pack "x")) . T.split (== ',')
  where
    slot bid = M.fromJust . L.find (>= departure) $ L.iterate (bid +) 0
    asBusId = M.fromJust . readMaybe @Int . T.unpack
    toBusTs bid = BusTs bid (slot bid)
