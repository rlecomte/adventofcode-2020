{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Puzzle.Day14 where

import qualified Data.Bits as B
import qualified Data.List as L
import qualified Data.Maybe as M
import Data.Monoid (Sum (..))
import Import hiding ((<|>), many, try)
import qualified RIO.Map as Map
import qualified RIO.Text as T
import Text.Parsec

newtype Mask = Mask String

data Mem = Mem Int64 Int64

data Op = MemOp Mem | MaskOp Mask

run :: Input -> RIO App ()
run input = do
  case solve input of
    (Right result) ->
      logInfo (fromString $ "Result " <> show result)
    (Left err) ->
      logError (fromString err)

solve :: Input -> Either String Int64
solve (Input input) = do
  ops <- mapLeft show $ traverse (parse lineParser "") (T.lines input)
  mems <- fmap (f <=< L.reverse) (foldM inputToList [] ops)
  let memMap = L.foldl' setMem Map.empty mems
  return $ getSum $ foldMap Sum $ Map.elems memMap
  where
    f (mask', mems) = fmap (applyMask mask') (L.reverse mems)
    setMem map' (Mem k v) = Map.insert k v map'

inputToList :: [(Mask, [Mem])] -> Op -> Either String [(Mask, [Mem])]
inputToList xs (MaskOp mask') = Right $ (mask', []) : xs
inputToList ((mask', mems) : xs) (MemOp mem) = Right $ (mask', mem : mems) : xs
inputToList _ _ = Left "unordered input file!"

applyMask :: Mask -> Mem -> Mem
applyMask (Mask m) (Mem k v) = Mem k $ L.foldl' setMaskBit v $ M.mapMaybe readInt $ L.zip [0 ..] $ L.reverse m
  where
    readInt = traverse (readMaybe @Int . char2Str)
    char2Str = (: [])
    setMaskBit y (idx, x) = if x > 0 then B.setBit y idx else B.clearBit y idx

lineParser :: Parsec Text () Op
lineParser = try (fmap MaskOp maskParser) <|> fmap MemOp memParser

maskParser :: Parsec Text () Mask
maskParser = do
  _ <- string "mask = "
  Mask <$> many alphaNum

memParser :: Parsec Text () Mem
memParser = do
  _ <- string "mem["
  n <- manyTill anyChar (string "] = ")
  n2 <- many anyChar
  let may = Mem <$> readMaybe n <*> readMaybe n2
  maybe (unexpected "fail to parse mem") pure may
