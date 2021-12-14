module Main where

diffs :: [Int] -> [Int]
diffs (x:y:xs)
 | x < y       = 1 : diffs (y : xs)
 | otherwise   = 0 : diffs (y : xs)
diffs _        = []

main :: IO ()
main = do
  rawInput <- getContents
  let answer = sum . diffs . map read . lines $ rawInput
  print answer
