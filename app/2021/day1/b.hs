module Main where

import qualified Data.Map as M

group :: [Int] -> [Int]
group [] = []
group xs = sum (take 3 xs) : group (tail xs)

diffs :: [Int] -> [Int]
diffs (x:y:xs)
 | x < y       = 1 : diffs (y : xs)
 | otherwise   = 0 : diffs (y : xs)
diffs _        = []

main :: IO ()
main = do
  rawInput <- getContents
  let measurements = group . map read . lines $ rawInput
  let answer = sum . diffs $ measurements
  print answer
