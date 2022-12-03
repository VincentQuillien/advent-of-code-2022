module Day1.Ex where

import Data.List (find, sortBy)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

main = do
  inputLines <- lines <$> readFile "day1.input"
  let groups = splitOn [""] inputLines
  let calories = (map . map) (read :: String -> Int) groups

  let top1CalorieSum = maximum $ sum <$> calories
  let top3CalorieSum = sum $ take 3 $ sortBy (flip compare) (sum <$> calories)
  return (top1CalorieSum, top3CalorieSum)
