module Day2.Ex where

import Data.List (find)
import Data.Maybe (fromJust)

win = [("C", "X"), ("A", "Y"), ("B", "Z")]

draw = [("A", "X"), ("B", "Y"), ("C", "Z")]

lose = [("B", "X"), ("C", "Y"), ("A", "Z")]

-- Part 1

pointsForOutcome round
  | round `elem` win = 6
  | round `elem` draw = 3
  | round `elem` lose = 0

pointsForMove "X" = 1
pointsForMove "Y" = 2
pointsForMove "Z" = 3

pointsRoundPart1 round = pointsForOutcome round + pointsForMove (snd round)

-- Part2

pointsForInstruction :: String -> [(String, String)] -> Int
pointsForInstruction move = pointsForMove . snd . fromJust . find ((== move) . fst)

pointsRoundPart2 :: (String, String) -> Int
pointsRoundPart2 (move, "X") = 0 + pointsForInstruction move lose
pointsRoundPart2 (move, "Y") = 3 + pointsForInstruction move draw
pointsRoundPart2 (move, "Z") = 6 + pointsForInstruction move win

-- Main

toTuple [x, y] = (x, y)

day2 = do
  inputLines <- lines <$> readFile "test.input"
  let rounds = toTuple . words <$> inputLines

  let part1 = sum $ pointsRoundPart1 <$> rounds
  let part2 = sum $ pointsRoundPart2 <$> rounds
  return (part1, part2)
