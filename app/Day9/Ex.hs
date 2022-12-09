module Day9.Ex where

import qualified Data.Set as Set (empty, insert, size)

parseMotion line = direction <$ [1 .. read steps :: Int]
  where
    (direction, steps) = splitAt 1 line

moveHead (x, y) "L" = (x - 1, y)
moveHead (x, y) "R" = (x + 1, y)
moveHead (x, y) "U" = (x, y + 1)
moveHead (x, y) "D" = (x, y - 1)
moveHead (x, y) _ = (x, y)

moveKnot (diffX, diffY) (x, y) = (x + signum diffX, y + signum diffY)

moveRope history tail [] = (Set.insert tail history, [])
moveRope history (headX, headY) (knot@(knotX, knotY) : rope) =
  let diffs@(diffX, diffY) = (headX - knotX, headY - knotY)
      updatedKnot = if abs diffX == 2 || abs diffY == 2 then moveKnot diffs knot else knot
      (updatedHistory, updatedRope) = moveRope history updatedKnot rope
   in (updatedHistory, updatedKnot : updatedRope)

processDirections history _ _ [] = history
processDirections history head rope (direction : directions) =
  let updatedHead = moveHead head direction
      (updatedHistory, updatedRope) = moveRope history updatedHead rope
   in processDirections updatedHistory updatedHead updatedRope directions

part1 = Set.size . processDirections Set.empty (0, 0) [(0, 0)]

part2 = Set.size . processDirections Set.empty (0, 0) ((0, 0) <$ [1 .. 9])

day9 file = do
  inputLines <- lines <$> readFile file
  let directions = concatMap parseMotion inputLines

  return (part1 directions, part2 directions)