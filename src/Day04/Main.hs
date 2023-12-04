module Main (main) where

import Parser
import Parser.Combinators
import Parser.Numeric

import           Data.Char     (isSpace)
import           Data.Map      (Map)
import qualified Data.Map as M
import           Data.Set      (Set)
import qualified Data.Set as S

data Card =
    Card Int (Set Int) (Set Int)

main :: IO ()
main = do
    content <- readFile "./src/Day04/input"
    let Right ("", cards) = runParser (many parseCard) content
    print (part1 cards)
    print (part2 cards)

part1 :: [Card] -> Int
part1 = sum . map value
    where
    value (Card _ drawn winners) =
        case length (S.intersection drawn winners) of
            0 -> 0
            w -> 2 ^ (w - 1)

part2 :: [Card] -> Int
part2 cards = go (M.fromList $ map (\c@(Card n _ _) -> (n, (c, 1))) cards) 1
    where
    go :: Map Int (Card, Int) -> Int -> Int
    go acc n =
        case M.lookup n acc of
            Nothing -> sum . map (snd . snd) . M.toList $ acc
            Just (Card _ drawn winners, copies) -> do
                let newCards = [n + 1 .. n + length (S.intersection drawn winners)]
                let acc' = foldr (M.alter (bump copies)) acc newCards
                go acc' (n+1)
                    where
                    bump :: Int -> Maybe (Card, Int) -> Maybe (Card, Int)
                    bump by (Just (card, copies')) = Just (card, copies' + by)

parseCard :: Parser String Card
parseCard = do
    _  <- list "Card"                                           <* pDropWhile isSpace
    n  <- pPositive <* list ":"                                 <* pDropWhile isSpace
    xs <- S.fromList <$> sepBy' (pDropWhile1 isSpace) pPositive <* pDropWhile isSpace
    _  <- list "|"                                              <* pDropWhile isSpace
    ys <- S.fromList <$> sepBy' (pDropWhile1 isSpace) pPositive <* pDropWhile isSpace
    pure $ Card n xs ys
