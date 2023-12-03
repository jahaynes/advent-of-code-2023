module Parser.Numeric (pPositive) where

import Data.Char (isDigit)

import Parser
import Parser.Combinators

pPositive :: Parser String Int
pPositive = read <$> pTakeWhile1 isDigit
