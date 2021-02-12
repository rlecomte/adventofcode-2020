{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Puzzle.Day8 where

import qualified Data.List as L
import Import hiding (try, (<|>))
import qualified RIO.Map as Map
import qualified RIO.Text as T
import Text.Parsec
import Prelude (read)

data Op = Nop | Acc Int | Jmp Int

newtype Pos = Pos Int deriving (Num, Eq, Ord)

newtype Result = Result Int deriving (Num, Show)

data S = S (Map Pos Op) Pos Result

run :: Input -> RIO App ()
run input = do
  let result = solve input
  logInfo (fromString $ "Result " <> show result)

solve :: Input -> Either ParseError Result
solve (Input input) = do
  ops <- traverse (parse parser "") (T.lines input)
  let m = Map.fromList $ L.zip (fmap Pos [0 ..]) ops
      s = S m (Pos 0) (Result 0)
      (S _ _ r) = fix frec s
  return r

frec :: (S -> S) -> S -> S
frec cb s@(S m p r) =
  let op = Map.lookup p m
      m' = Map.delete p m
   in case op of
        (Just Nop) -> cb $ S m' (p + 1) r
        (Just (Acc x)) -> cb $ S m' (p + 1) (r + Result x)
        (Just (Jmp x)) -> cb $ S m' (p + Pos x) r
        _ -> s

parser :: Parsec Text () Op
parser =
  let nop = Nop <$ string "nop"
      acc = Acc <$> (string "acc" >> digits)
      jmp = Jmp <$> (string "jmp" >> digits)
   in try nop <|> try acc <|> jmp

digits :: Parsec Text () Int
digits = do
  spaces
  sign <- oneOf "-+"
  n <- read <$> many1 digit
  return $ case sign of
    '-' -> negate n
    _ -> n
