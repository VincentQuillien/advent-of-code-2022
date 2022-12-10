module Day10.Ex where

import Data.List (intercalate)
import Data.List.Split (chunksOf)

parseLine ("addx", n) = [0, read n :: Int]
parseLine _ = [0]

getRegisters = scanl (+) 1

getSignals = zipWith (*) [0 ..] . getRegisters

signalsAtInterval n instructions =
  map (signals !!) (take n (iterate (+ 40) 20))
  where
    signals = getSignals instructions

part1 = sum . signalsAtInterval 6

draw cycle register =
  if cycle `elem` [register - 1 .. register + 1] then '#' else '.'

part2 = concatMap ('\n' :) . chunksOf 40 . zipWith draw (cycle [0 .. 39]) . getRegisters

day10 file = do
  inputLines <- lines <$> readFile file
  let instructions = concatMap (parseLine . splitAt 4) inputLines
  putStr (part2 instructions)
  return (part1 instructions)