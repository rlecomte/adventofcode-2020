{-# LANGUAGE NoImplicitPrelude #-}

module Puzzle.Day11 where

import qualified Data.List as L
import Data.Vector ((!), (!?))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Import
import qualified RIO.Text as T

data Pos = Occupied | Empty | Floor deriving (Show, Eq)

run :: Input -> RIO App ()
run input = do
  let r = solve input
  logInfo (fromString $ "Result " <> show r)
  return ()

solve :: Input -> Int
solve = uncurry compute . parseLayout

compute :: Int -> Vector Pos -> Int
compute x = V.length . V.filter (== Occupied) . f
  where
    f v =
      let v' = nextLayout x v
       in bool (f v') v (v == v')

nextLayout :: Int -> Vector Pos -> Vector Pos
nextLayout lineSize state = V.modify f state
  where
    f v = for_ [0 .. (V.length state - 1)] $ \x -> do
      let pos = state ! x
      MV.write v x (g x pos)
    g x s =
      let neighbors = countNeighbors x
       in case s of
            Floor -> Floor
            Empty -> bool Empty Occupied (neighbors == 0)
            Occupied -> bool Occupied Empty (neighbors > 3)
    countNeighbors pos =
      L.length $ L.filter (== Occupied)
        $ mapMaybe (state !?)
        $ catMaybes
          [p1 pos, Just (p2 pos), p3 pos, p4 pos, p5 pos, p6 pos, Just (p7 pos), p8 pos]
    startLine pos = pos - mod pos lineSize
    endLine pos = startLine pos + lineSize - 1
    p1 pos = if startLine pos == pos then Nothing else Just (pos - lineSize - 1)
    p2 pos = pos - lineSize
    p3 pos = if endLine pos == pos then Nothing else Just (pos - lineSize + 1)
    p4 pos = if startLine pos == pos then Nothing else Just (pos - 1)
    p5 pos = if endLine pos == pos then Nothing else Just (pos + 1)
    p6 pos = if startLine pos == pos then Nothing else Just (pos + lineSize - 1)
    p7 pos = pos + lineSize
    p8 pos = if endLine pos == pos then Nothing else Just (pos + lineSize + 1)

parseLayout :: Input -> (Int, Vector Pos)
parseLayout (Input input) = (lineSize input, layout input)
  where
    f '.' = Just Floor
    f '#' = Just Occupied
    f 'L' = Just Empty
    f '\n' = Nothing
    f _ = error "oops"
    lineSize = T.length . L.head . T.lines
    layout = V.fromList . mapMaybe f . T.unpack
