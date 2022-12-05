module Day5.Ex where

import Data.List (transpose)
import Data.List.Index (updateAt)
import Data.List.Split (chunksOf, splitOn)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

parseStacks = map (filter (/= ' ')) . transpose . map (map head . chunksOf 4 . tail)

parseMoves = map (mapMaybe readMaybe . splitOn " ")

moveWith craneOperation stacks [count, from, to] =
  updateAt (from - 1) (\_ -> Just rest) $
    updateAt (to - 1) (Just . craneOperation toMove) stacks
  where
    (toMove, rest) = splitAt count (stacks !! (from - 1))

moveAll craneOperation stacks = map head . foldl (moveWith craneOperation) stacks

stackEach = flip $ foldl (flip (:))

part1 = moveAll stackEach

part2 = moveAll (++)

day5 file = do
  inputLines <- lines <$> readFile file
  let [stacksBlock, movesBlock] = splitOn [""] inputLines
  let (stacks, moves) = (parseStacks stacksBlock, parseMoves movesBlock)

  return (part1 stacks moves, part2 stacks moves)
