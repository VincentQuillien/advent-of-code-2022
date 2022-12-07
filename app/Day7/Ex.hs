module Day7.Ex where

import Data.List (find, sort)
import Text.Read (readMaybe)

data Tree = Tree {size :: Int, name :: String, children :: [Tree]} deriving (Show)

emptyTree = Tree {size = 0, name = "/", children = []}

parseInput :: [[String]] -> Tree -> ([[String]], Tree)
parseInput [] tree = ([], tree)
parseInput (["$", "cd", ".."] : lines) tree = (lines, tree)
parseInput (["$", "cd", dirName] : lines) tree@Tree {..} =
  let (restLines, subTree) = parseInput lines emptyTree {name = dirName}
   in parseInput restLines tree {children = subTree : children}
parseInput ([n, _] : lines) tree@Tree {..} =
  case readMaybe n :: Maybe Int of
    Just fileSize -> parseInput lines tree {size = size + fileSize}
    Nothing -> parseInput lines tree
parseInput (_ : lines) tree = (lines, tree)

cumulativeSize Tree {size = dirSize, ..} =
  let newChildren = map cumulativeSize children
   in Tree {size = dirSize + sum (map size newChildren), children = newChildren, ..}

flatten Tree {children = [], ..} = [size]
flatten Tree {..} = size : concatMap flatten children

part1 = sum . filter (<= 100000)

part2 directories = find (\x -> freespace + x >= 30000000) (sort directories)
  where
    freespace = 70000000 - head directories

day7 file = do
  inputLines <- lines <$> readFile file
  let tree = snd $ parseInput (tail $ words <$> inputLines) emptyTree
  let directories = flatten (cumulativeSize tree)

  return (part1 directories, part2 directories)
