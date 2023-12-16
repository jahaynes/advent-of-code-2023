module Parser.Numeric where

import Control.Applicative ((<|>))
import Data.Char           (isDigit)

import Parser
import Parser.Combinators

pInt :: Parser String Int
pInt = pNegative <|> pPositive

pPositive :: Parser String Int
pPositive = read <$> pTakeWhile1 isDigit

pNegative :: Parser String Int
pNegative = do
    n <- such (=='-') *> pPositive
    pure (-n)