{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Day8.Ex where

import Control.Lens (makeLenses)
import Control.Lens.Lens (Lens')
import Control.Lens.Setter (set)
import Data.Char (digitToInt)
import Data.List (transpose)

data Tile = Tile
  { _height :: Int,
    _left :: Bool,
    _right :: Bool,
    _top :: Bool,
    _bottom :: Bool
  }
  deriving (Show)

makeLenses ''Tile

createTile height = Tile {_height = height, _left = True, _right = True, _top = True, _bottom = True}

-- Part1
lineVisibilities direction max [] = []
lineVisibilities direction max (tile : restTiles)
  | _height tile <= max = set direction False tile : lineVisibilities direction max restTiles
  | otherwise = tile : lineVisibilities direction (_height tile) restTiles

lineVisibilitiesFromEnd direction max = reverse . lineVisibilities direction max . reverse

linesVisibilities direction reverseDirection =
  map (lineVisibilitiesFromEnd reverseDirection (-1) . lineVisibilities direction (-1))

gridVisibilities = linesVisibilities top bottom . transpose . linesVisibilities left right

visibleTiles = length . filter (\Tile {..} -> _left || _right || _top || _bottom) . concat

part1 = visibleTiles . gridVisibilities

day8 file = do
  inputLines <- lines <$> readFile file
  let tiles = (map . map) (createTile . digitToInt) inputLines

  return (part1 tiles)