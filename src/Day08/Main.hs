module Main (main) where

import Parser
import Parser.Combinators

import           Control.Applicative  ((<|>))
import           Data.Map.Strict as M ((!), Map, fromList, keys)
import           Data.Vector          (Vector)
import qualified Data.Vector as V

data Dir =
    L | R

data Node =
    Node !Char !Char !Char
        deriving (Eq, Ord)

main :: IO ()
main = do
    content <- readFile "./src/Day08/input"
    let Right ("", (dirs, elems)) = runParser parseDocument content
    let elemMap = fromList elems
    print $ part1 elemMap dirs
    print $ part2 elemMap dirs

part1 :: Map Node (Node, Node) -> [Dir] -> Int
part1 elemMap = length
              . takeWhile (/= Node 'Z' 'Z' 'Z')
              . run elemMap (Node 'A' 'A' 'A')

run :: Map Node (Node, Node) -> Node -> [Dir] -> [Node]
run elemMap startNode dirs = go startNode (cycle dirs)
    where
    go node (d:ds) =
        let (l, r) = elemMap ! node
        in node : go (case d of L -> l; R -> r) ds

part2 :: Map Node (Node, Node) -> [Dir] -> Int
part2 elemMap dirs = V.foldl' lcm 1
                   $ V.map cycleLength startNodes

    where
    cycleLength :: Node -> Int
    cycleLength startNode = (\[a,b] -> b-a) . map fst . take 2 . filter (\(_,n) -> isEndNode n) . zip [0..] . run elemMap startNode $ dirs

    startNodes :: Vector Node
    startNodes = V.fromList
               . filter isStartNode
               . keys
               $ elemMap

    isStartNode :: Node -> Bool
    isStartNode (Node _ _ c) = c == 'A'

    isEndNode :: Node -> Bool
    isEndNode (Node _ _ c) = c == 'Z'

parseDocument :: Parser String ([Dir], [(Node, (Node, Node))])
parseDocument = (,) <$> many1 parseDir <* list "\n\n"
                    <*> many1 parseElem

    where
    parseDir :: Parser String Dir
    parseDir = (L <$ list "L")
           <|> (R <$ list "R")

    parseElem :: Parser String (Node, (Node, Node))
    parseElem = do
        a <- parseNode <* list " = ("
        b <- parseNode <* list ", "
        c <- parseNode <* list ")\n"
        pure (a, (b, c))

    parseNode :: Parser String Node
    parseNode = Node <$> one <*> one <*> one