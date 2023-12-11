module Main (main) where

import Parser
import Parser.Combinators

import Data.Char (isDigit, isSpace)
import Data.List (group, partition, sort, sortBy)
import Data.Map  ((!), fromList)

data Part = Part1 | Part2

data Type = HC | P1 | P2 | K3 | FH | K4 | K5 deriving (Eq, Ord)

newtype Bid = Bid Int deriving (Eq, Ord)

data Hand = Hand !Type ![Int] String !Bid deriving (Eq, Ord)

newtype Rank = Rank Int

main :: IO ()
main = do

    content <- readFile "./src/Day07/input"

    let Right ("", part1Hands) = runParser (many1 (parseHand Part1)) content
    print $ score part1Hands

    let Right ("", part2Hands) = runParser (many1 (parseHand Part2)) content
    print $ score part2Hands

score :: [Hand] -> Int
score = sum
      . map (\(Rank r, Hand _ _ _ (Bid b)) -> r * b)
      . zip ranks
      . sort
    where
    ranks = Rank <$> [1..]

parseHand :: Part -> Parser String Hand
parseHand part = do
    hand <- pTakeWhile1 (not . isSpace) <* pDropWhile1 isSpace
    bid  <- read <$> pTakeWhile1 isDigit <* pDropWhile1 isSpace
    pure $ Hand (classify hand) (map (labelScores !) hand) hand (Bid bid)

    where
    labelScores = fromList $ zip labels [1..]

        where
        labels =
            case part of
                Part1 -> "23456789TJQKA"
                Part2 -> "J23456789TQKA"

    classify hand =

        let (jokers, nonJokers) =
                case part of
                    Part1 -> ([], hand)
                    Part2 -> partition (=='J') hand

        in case sortBy (flip compare) . map length . group . sort $ nonJokers of
            []     -> K5
            (x:xs) ->
                case x+length jokers:xs of
                    [5]         -> K5
                    [4,1]       -> K4
                    [3,2]       -> FH
                    [3,1,1]     -> K3
                    [2,2,1]     -> P2
                    [2,1,1,1]   -> P1
                    [1,1,1,1,1] -> HC
                    _           -> undefined
