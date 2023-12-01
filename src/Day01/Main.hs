module Main (main) where

import Parser              (Parser (..))
import Parser.Combinators  (list, many1, one, such)

import Control.Applicative ((<|>))
import Data.Char           (isDigit)
import Data.Maybe          (catMaybes)

main :: IO ()
main = do
    content <- readFile "./src/Day01/input"
    print (part1 content)
    print (part2 content)

part1 :: String -> Int
part1 = sum . map calibrationValue . lines

    where
    calibrationValue line =
        let digits = filter isDigit line
        in read [head digits, last digits]

part2 :: String -> Int
part2 = sum . map calibrationValue . lines

    where
    calibrationValue line =
        let Right ("", forward)  = runParser (parseDigits id) line
            Right ("", backward) = runParser (parseDigits reverse) (reverse line)
        in read [head forward, head backward]

    parseDigits f = catMaybes <$> many1 token

        where
        token = Just '9' <$  list (f "nine")
            <|> Just '8' <$  list (f "eight")
            <|> Just '7' <$  list (f "seven")
            <|> Just '6' <$  list (f "six")
            <|> Just '5' <$  list (f "five")
            <|> Just '4' <$  list (f "four")
            <|> Just '3' <$  list (f "three")
            <|> Just '2' <$  list (f "two")
            <|> Just '1' <$  list (f "one")
            <|> Just     <$> such isDigit
            <|> Nothing  <$  one
