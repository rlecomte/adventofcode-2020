{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Puzzle.Day7 where

import qualified Data.List as L
import Import hiding (try, (<|>))
import qualified RIO.Map as Map
import qualified RIO.Text as T
import Text.Parsec
import Prelude (read)

newtype Bag = Bag String deriving (Show, Eq, Ord)

type BagEntry = (Bag, [Bag])

run :: Input -> RIO App ()
run input = do
  let result = solve input
  logInfo (fromString $ "Result " <> show result)

solve :: Input -> Either ParseError Int
solve (Input input) = do
  bagMap <- runParse (T.lines input)
  let bags = L.filter (Bag "shiny gold" /=) $ Map.keys bagMap
  return $ L.length $ L.filter (containShinyBag bagMap) bags

containShinyBag :: Map Bag [Bag] -> Bag -> Bool
containShinyBag _ (Bag "shiny gold") = True
containShinyBag m b = isJust . L.find (containShinyBag m) . fromMaybe [] $ Map.lookup b m

runParse :: [Text] -> Either ParseError (Map Bag [Bag])
runParse = fmap Map.fromList . traverse (parse lineParser "")

-- light red bags contain 1 bright white bag, 2 muted yellow bags.
lineParser :: Parsec Text () BagEntry
lineParser = do
  bag <- parseBag
  _ <- spaces >> string "contain"
  innerBags <- try noOtherBag <|> sepBy1 innerBag (string ",")
  return (bag, innerBags)

parseBag :: Parsec Text () Bag
parseBag =
  let bagSuffix = try (string "bags") <|> string "bag"
   in Bag <$> manyTill anyChar (try (spaces >> bagSuffix))

innerBag :: Parsec Text () Bag
innerBag = spaces >> digits >> spaces >> parseBag

digits :: Parsec Text () Int
digits = fmap read (many1 digit)

noOtherBag :: Parsec Text () [Bag]
noOtherBag = spaces >> string "no other bags" $> []
