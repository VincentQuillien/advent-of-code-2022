module Day6.Ex where

import Data.List (nub)

findMarker markerSize processedChars signal
  | length (nub $ take markerSize signal) == markerSize = processedChars + markerSize
  | otherwise = findMarker markerSize (processedChars + 1) (tail signal)

part1 = findMarker 4 0

part2 = findMarker 14 0

day6 file = do
  input <- readFile file
  return (part1 input, part2 input)