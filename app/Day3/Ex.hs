module Day3.Ex where

import Data.List.Split (chunksOf, splitOn)
import qualified Data.Set as Set

-- Part 1
(lowercase, uppercase) = (Set.fromList ['a' .. 'z'], Set.fromList ['A' .. 'Z'])

pointsForLetter l
  | l `Set.member` lowercase = fromEnum l - 96
  | l `Set.member` uppercase = fromEnum l - 38

rucksackPriority (left, right) = sum $ Set.map pointsForLetter (left `Set.intersection` right)

splitInHalf s =
  let (left, right) = splitAt (length s `div` 2) s
   in (Set.fromList left, Set.fromList right)

part1 = sum . map (rucksackPriority . splitInHalf)

-- Part 2
groupBadgePriority :: [String] -> Int
groupBadgePriority = sum . Set.map pointsForLetter . foldl1 Set.intersection . map Set.fromList

part2 = sum . map groupBadgePriority . chunksOf 3

-- Main
day3 file = do
  rucksacks <- lines <$> readFile file
  return (part1 rucksacks, part2 rucksacks)
