module Main (main) where

import Data.List

data Movement = N | S | E | W
              deriving (Show)

type Position = (Int, Int)

move :: Position -> Movement -> Position
move (x, y) N = (x, y + 1)
move (x, y) S = (x, y - 1)
move (x, y) E = (x - 1, y)
move (x, y) W = (x + 1, y)

moves :: [Movement] -> [Position]
moves = scanl move (0, 0)

parse :: Char -> Movement
parse '>' = W
parse '<' = E
parse '^' = N
parse 'v' = S
parse _   = error "bad input"

split :: [a] -> ([a], [a])
split = go [] []
  where
    go acc0 acc1 []             = (reverse acc0, reverse acc1)
    go acc0 acc1 (x0 : x1 : xs) = go (x0 : acc0) (x1 : acc1) xs
    go acc0 acc1 [x0]           = go (x0 : acc0) acc1 []

main :: IO ()
main = do
  input <- map parse <$> getLine
  let (inputa, inputb) = split input
  print $ length $ nub $ moves input
  print $ length $ nub $ moves inputa ++ moves inputb
