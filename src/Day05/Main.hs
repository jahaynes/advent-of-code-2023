module Main (main) where

import Parsing
import Types

import Parser (runParser)

import           Data.List       (sort)
import           Data.List.Split (chunksOf)
import           Data.Map        ((!), Map)
import qualified Data.Map as M
import           Data.Maybe      (catMaybes)

main :: IO ()
main = do
    content <- readFile "./src/Day05/input"
    case runParser parseSeedsAndEntries content of
        Right ("", (seeds, entries)) -> do
            let almenac = buildAlmenac entries
            print (part1 almenac seeds)
            print (part2 almenac seeds)
        _ -> error "Failed to parse"

part1 :: Map String (String, [(Int, Int, Int)]) -> Seeds -> Int
part1 almenac (Seeds seeds) =
    minimum $ map (convert "seed" "location") seeds

    where
    convert start final n
        | start == final = n
        | otherwise = do
            let (dest, loHiOffs) = almenac ! start
            case filter (\(lo, hi, _) -> n >= lo && n <= hi) loHiOffs of
                []            -> convert dest final n
                [(_, _, off)] -> convert dest final (n+off)
                _             -> undefined

part2 :: Map String (String, [(Int, Int, Int)]) -> Seeds -> Int
part2 almenac (Seeds seeds) = minimum
                            . map convertSeedPair
                            . chunksOf 2
                            $ seeds

    where
    convertSeedPair [lo, range] = convertAllRanges "seed" "location" lo (lo + range - 1)
    convertSeedPair           _ = error "Invalid seed pair"

    convertAllRanges start final a b
        | start == final = a
        | otherwise =
            let (dest, loHiOffs) = almenac ! start
            in minimum . map (\(ta, tb) -> convertAllRanges dest final ta tb)
                       . convertRange a b
                       . sort
                       $ loHiOffs

convertRange :: Int -> Int -> [(Int, Int, Int)] -> [(Int, Int)]
convertRange a b [] = [(a,b)]
convertRange a b ((lo,hi,off):loHiOffs) =
    let results = check
        ins     = applyOffset <$> getIn results
        next    = case getRight results of
                       Nothing         -> []
                       Just (roa, rob) -> convertRange roa rob loHiOffs
    in catMaybes [getLeft results, ins] ++ next

    where
    applyOffset (a', b') = (off + a', off + b')

    check | b < lo    = empty { getLeft  = Just (a, b) }
          | a > hi    = empty { getRight = Just (a, b) }
          | a < lo    = if b > hi
                            then Contained { getLeft  = Just (a, lo-1)
                                           , getIn    = Just (lo, hi)
                                           , getRight = Just (hi+1, b) }
                            else Contained { getLeft  = Just (a, lo-1)
                                           , getIn    = Just (lo, b)
                                           , getRight = Nothing }
          | b > hi    = Contained { getLeft  = Nothing
                                  , getIn    = Just (a, hi)
                                  , getRight = Just (hi+1, b) }
          | otherwise = empty { getIn = Just (a, b) }

buildAlmenac :: [Entry] -> Map String (String, [(Int, Int, Int)])
buildAlmenac = M.fromListWith (\(d1, xs) (d2, ys) -> if d1 == d2 then (d1, ys ++ xs) else error "Bad almenac")
             . map build
    where
    build :: Entry -> (String, (String, [(Int, Int, Int)]))
    build e =
        let lo     = getSourceRangeStart e
            hi     = lo + getMappings e - 1
            offset = getDestRangeStart e - getSourceRangeStart e
        in (getSource e, (getDest e, [(lo, hi, offset)]))
