{-# LANGUAGE NoImplicitPrelude #-}

module Puzzle.Day12 where

import qualified Data.List as L
import Import
import qualified RIO.Text as T
import Prelude (read)

data Dir = N | S | E | W

data Nav = NN Int | NS Int | NE Int | NW Int | L Int | R Int | F Int

data Pos = Pos {direction :: Dir, ns :: Int, ew :: Int}

run :: Input -> RIO App ()
run (Input input) = do
  let dist = solve . fmap parse . T.lines $ input
  logInfo (fromString $ "Result " <> show dist)

solve :: [Nav] -> Int
solve navs = let pos = L.foldl' solveStep (Pos E 0 0) navs in abs (ns pos) + abs (ew pos)

solveStep :: Pos -> Nav -> Pos
solveStep p (NN i) = p {ns = ns p + i}
solveStep p (NS i) = p {ns = ns p - i}
solveStep p (NE i) = p {ew = ew p + i}
solveStep p (NW i) = p {ew = ew p - i}
solveStep p (L i) =
  let d = foldl' (const . turnLeft) (direction p) [1 .. (div i 90)]
   in p {direction = d}
solveStep p (R i) =
  let d = foldl' (const . turnRight) (direction p) [1 .. (div i 90)]
   in p {direction = d}
solveStep p (F i) = solveStep p (forward (direction p) i)

forward :: Dir -> Int -> Nav
forward N = NN
forward S = NS
forward E = NE
forward W = NW

turnRight :: Dir -> Dir
turnRight N = E
turnRight S = W
turnRight E = S
turnRight W = N

turnLeft :: Dir -> Dir
turnLeft N = W
turnLeft S = E
turnLeft E = N
turnLeft W = S

parse :: Text -> Nav
parse t =
  let (prefix, value) = T.splitAt 1 t
      v = read $ T.unpack value
   in case T.unpack prefix of
        "N" -> NN v
        "S" -> NS v
        "E" -> NE v
        "W" -> NW v
        "L" -> L v
        "R" -> R v
        "F" -> F v
        _ -> error (T.unpack t)
