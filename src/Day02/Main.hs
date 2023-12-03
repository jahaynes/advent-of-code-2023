module Main (main) where

import Parser
import Parser.Combinators
import Parser.Numeric

import           Data.Char (isLetter, isSpace)
import           Data.Map  (Map)
import qualified Data.Map as M

data Game =
    Game !Int [Round] deriving Show

newtype Round =
    Round (Map String Int) deriving Show

main :: IO ()
main = do
    content <- readFile "./src/Day02/input"
    let Right ("", games) = runParser parseGames content
    print (part1 games)
    print (part2 games)

part1 :: [Game] -> Int
part1 = sum . map (\(Game n _) -> n) . filter isGamePossible
    where
    isGamePossible (Game _ rounds) =
        all (\(Round r) -> all isDrawPossible (M.toList r)) rounds
        where
        isDrawPossible (colour, number) =
            case M.lookup colour possible of
                Nothing -> False
                Just nc -> number <= nc
        possible = M.fromList [("red", 12), ("green", 13), ("blue", 14)]

part2 :: [Game] -> Int
part2 = sum . map (product . M.elems . minForGame)
    where
    minForGame (Game _ rounds) =
        foldr (\(Round r) -> M.unionWith max r) mempty rounds

parseGames :: Parser String [Game]
parseGames = sepBy "\n" parseGame
          <* pDropWhile isSpace
          <* eof

    where
    parseGame =
        Game <$> (list "Game " *> pPositive <* list ": ")
             <*> (sepBy "; " parseRound)

    parseRound =
        Round . M.fromList <$> sepBy ", " parseDraw

    parseDraw = do
        n      <- pPositive <* pDropWhile isSpace
        colour <- pTakeWhile1 isLetter
        pure (colour, n)
