module Day4.Ex where

import Data.List.Split (splitOn)

isSubset [x, y] [x', y'] = x >= x' && y <= y'

containsSubset [left, right] = left `isSubset` right || right `isSubset` left

part1 = length . filter id . map containsSubset

anyOverlap [[x, y], [x', y']] = y >= x' && y' >= x

part2 = length . filter id . map anyOverlap

makePair :: [Char] -> [[Int]]
makePair = map (map read . splitOn "-") . splitOn ","

day4 file = do
  inputLines <- lines <$> readFile file
  let pairs = makePair <$> inputLines

  return (part1 pairs, part2 pairs)
