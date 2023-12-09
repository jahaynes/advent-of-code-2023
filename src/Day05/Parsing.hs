module Parsing where

import Types

import Parser
import Parser.Combinators
import Parser.Numeric

import Control.Monad (replicateM)
import Data.Char     (isAlpha, isSpace)

parseSeedsAndEntries :: Parser String (Seeds, [Entry])
parseSeedsAndEntries = do
    seeds    <- parseSeeds
    mappings <- many1 (list "\n" *> parseMapping)
    pure (seeds, concat mappings)

    where
    parseSeeds :: Parser String Seeds
    parseSeeds = Seeds <$> (list "seeds:" *> many (pDropWhile1 isSpace *> pPositive) <* list "\n")

    parseMapping :: Parser String [Entry]
    parseMapping = do
        source <- pTakeWhile isAlpha <* list "-to-"
        dest   <- pTakeWhile isAlpha <* list " map:\n"
        rows   <- many1 (replicateM 3 (pDropWhile isSpace *> pPositive) <* list "\n")
        pure $ map (\r -> let [a,b,c] = r in Entry source dest a b c) rows
