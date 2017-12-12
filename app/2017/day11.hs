module Main (main) where

import Data.List

type Cube = (Int, Int, Int)

data Direction = NE | SE | S | SW | NW | N
               deriving (Show)

move :: Direction -> Cube -> Cube
move N (x, y, z)  = (x, y + 1, z - 1)
move S (x, y, z)  = (x, y - 1, z + 1)
move NE (x, y, z) = (x + 1, y, z - 1)
move SW (x, y, z) = (x - 1, y, z + 1)
move NW (x, y, z) = (x - 1, y + 1, z)
move SE (x, y, z) = (x + 1, y - 1, z)

moves :: [Direction] -> Cube
moves = foldl' (flip move) (0, 0, 0)

distances :: [Direction] -> Int
distances = fst . foldl' move' (0, (0, 0, 0))
  where
    move' (acc, a) dir = let b = move dir a
                             d = distance b
                         in (max d acc, b)

manhattan :: Cube -> Int
manhattan (x, y, z) = abs x + abs y + abs z

distance :: Cube -> Int
distance c0 = manhattan c0 `div` 2

split :: String -> [String]
split s = let (l, r) = break (== ',') s
          in case r of
               [] -> [l]
               r  -> l : (split $ drop 1 r)

parse :: String -> Direction
parse "ne" = NE
parse "nw" = NW
parse "n"  = N
parse "sw" = SW
parse "se" = SE
parse "s"  = S
parse _    = error "bad input"

main :: IO ()
main = do
  dirs <- map parse . split <$> getLine
  print $ distance $ moves dirs
  print $ distances dirs
