module Main where

import Data.List

wordsBy :: (Char -> Bool) -> String -> [String]
wordsBy p s = case dropWhile p s of
                "" -> []
                s' -> w : wordsBy p s''
                  where (w, s'') = break p s'

solve :: [Int] -> Int
solve points =
  let m = median points
  in minimum [ sum $ map (\v -> abs $ m - v) points
             , sum $ map (\v -> abs $ m + 1 - v) points
             ]

median :: [Int] -> Int
median xs
  | odd len   = sorted !! mid
  | otherwise = mean $ take 2 $ drop (mid - 1) sorted
  where
    mid = div len 2
    len = length xs
    sorted = sort xs
    mean [a, b] = floor $ (fromIntegral a + fromIntegral b) / 2

main :: IO ()
main = do
  input <- map read . wordsBy (== ',') <$> getContents
  let answer = solve input
  print answer
