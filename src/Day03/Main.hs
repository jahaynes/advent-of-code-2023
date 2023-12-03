module Main (main) where

import Parser
import Parser.Combinators

import           Control.Applicative ((<|>))
import           Data.Char           (isDigit)
import           Data.Either         (partitionEithers)
import           Data.List           (nub)
import           Data.Maybe          (catMaybes, mapMaybe)
import           Data.Map            (Map)
import qualified Data.Map as M

data Pos =
    Pos !Int !Int
        deriving (Eq, Ord, Show)

main :: IO ()
main = do

    contents <- readFile "./src/Day03/input"

    let Right (_, parsed) = runParser parser $ annotatePositions contents
    let (symbols, parts) = partitionEithers $ catMaybes parsed
    let (partLookup, posLookup) = buildLookups parts
    let resolver = resolve partLookup posLookup

    print (part1 resolver symbols)
    print (part2 resolver symbols)

part1 :: (a -> [Int]) -> [a] -> Int
part1 resolver =
    sum . map (sum . resolver)

part2 :: ((a, Char) -> [Int]) -> [(a, Char)] -> Int
part2 resolver =
    sum . map product
        . filter (\x -> length x == 2)
        . map resolver
        . filter (\s -> snd s == '*')

resolve :: Map Int String -> Map Pos Int -> (Pos, Char) -> [Int]
resolve partLookup posLookup = map read
                             . mapMaybe (`M.lookup` partLookup)
                             . nub
                             . mapMaybe (`M.lookup` posLookup)
                             . adjacentPositions
                             . fst

buildLookups :: [(Pos, [a])] -> (Map Int [a], Map Pos Int)
buildLookups parts =
    let idParts = zip [0..] parts
    in (buildPartLookup idParts, buildPosLookup idParts)

    where
    buildPartLookup =
        M.fromList . map (\(i, (_, part)) -> (i, part))

    buildPosLookup =
        M.fromList . concatMap (\(i, (Pos r c, part)) -> map (\c' -> (Pos r c', i)) [c..(c+length part - 1)])

adjacentPositions :: Pos -> [Pos]
adjacentPositions (Pos r c) = do
    r' <- [r-1..r+1]
    c' <- [c-1..c+1]
    pure $ Pos r' c'

parser :: Show a => Parser [(a, Char)] [Maybe (Either (a, Char) (a, [Char]))]
parser = many (dots <|> part <|> symb <|> other) <* eof

    where
    dots =
        Nothing <$ many1 (such (\x -> snd x == '.'))

    part = do
        digits <- many1 (such (isDigit . snd))
        let ((pos, _):_) = digits
        let xs = map (\(_,b) -> b) digits
        pure . Just . Right $ (pos, xs)

    symb =
        Just . Left <$> such (\x -> not (isDigit (snd x) && not (snd x == '.')))

    other = do
        o <- one
        error $ show o

annotatePositions :: [Char] -> [(Pos, Char)]
annotatePositions content = do
    (rowNum, row) <- zip [0..] (lines content)
    (colNum, col) <- zip [0..] row
    pure (Pos rowNum colNum, col)