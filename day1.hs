import Data.List (sortBy)
import Data.List.Split

main = do
  inputLines <- lines <$> readFile "day1.input"
  let groups = splitOn [""] inputLines
  let calories = (map . map) (read :: String -> Int) groups

  let top1CalorieSum = maximum $ sum <$> calories
  let top3CalorieSum = sum $ take 3 $ sortBy (flip compare) (sum <$> calories)
  return (top1CalorieSum, top3CalorieSum)
