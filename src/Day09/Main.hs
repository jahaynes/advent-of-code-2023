module Main (main) where

import Parser
import Parser.Combinators
import Parser.Numeric

import Data.List (unfoldr)

data Part = Part1 | Part2

main :: IO ()
main = do
    content <- readFile "./src/Day09/input"
    let Right ("", inputs) = runParser parseLayers content
    print . sum $ map (grownValue Part1) inputs
    print . sum $ map (grownValue Part2) inputs

grownValue :: Part -> [Int] -> Int
grownValue part =
    case part of
        Part1 -> last . head . grow . allLayers
        Part2 -> head . head . grow . allLayers

    where
    allLayers = unfoldr go
        where
        go layer | all (==0) layer = Nothing
                 | otherwise       = Just (layer, zipWith (-) (tail layer) layer)

    grow =
        case part of
            Part1 -> reverse . map reverse . grow' . map reverse . reverse
            Part2 -> reverse               . grow'               . reverse
        where
        grow'                  [] = error "Invalid input"
        grow'        (base:above) = base : grow'' (head base) above
        grow'' _               [] = []
        grow'' _ ([]:_)           = error "Invalid layer"
        grow'' x ((l:ayer):above) = (x':l:ayer) : grow'' x' above
            where
            x' = case part of
                     Part1 -> l + x
                     Part2 -> l - x

parseLayers :: Parser [Char] [[Int]]
parseLayers = sepBy' (such (=='\n'))
                  (sepBy' (pTakeWhile1 (==' ')) pInt)
                      <* such (=='\n')
