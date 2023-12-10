module Main (main) where

import Parser
import Parser.Combinators
import Parser.Numeric

import Data.Char                 (isDigit, isSpace)
import GHC.Float.RealFracMethods (ceilingDoubleInt, floorDoubleInt)

data Race =
    Race { getTime   :: !Int
         , getRecord :: !Int
         }

main :: IO ()
main = do

    content <- readFile "./src/Day06/input"
    let Right ("", races) = runParser parser1 content
    print (solve races)

    let Right ("", race) = runParser parser2 content
    print (solve [race])

solve :: [Race] -> Int
solve = product . map numRecordBeaters

    where
    numRecordBeaters race =
        let (lo, hi) = findRecordInputs race
        in hi - lo + 1

    findRecordInputs race =
        let t      = fromIntegral $ getTime race
            record = fromIntegral $ getRecord race
            part   = sqrt(t ^ (2::Int) - 4 * record)
            lo     = ceilingDoubleInt $ (t - part) / 2
            hi     = floorDoubleInt $ (t + part) / 2
        in ( if findDistance race lo == getRecord race
                then lo + 1
                else lo
           , if findDistance race hi == getRecord race
                then hi - 1
                else hi )

    findDistance race held = getTime race * held
                           - held ^ (2 :: Int)

parser1 :: Parser String [Race]
parser1 = do
    times   <- list "Time:"     *> many1 (pDropWhile isSpace *> pPositive) <* list "\n"
    records <- list "Distance:" *> many1 (pDropWhile isSpace *> pPositive) <* list "\n"
    eof
    pure $ zipWith Race times records

parser2 :: Parser String Race
parser2 = do
    times   <- list "Time:"     *> many1 (pDropWhile isSpace *> pTakeWhile1 isDigit) <* list "\n"
    records <- list "Distance:" *> many1 (pDropWhile isSpace *> pTakeWhile1 isDigit) <* list "\n"
    eof
    pure $ Race (read $ concat times) (read $ concat records)