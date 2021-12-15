module Main where

import Debug.Trace
import Data.List
import Data.Function

wordsBy :: (Char -> Bool) -> String -> [String]
wordsBy p s = case dropWhile p s of
                "" -> []
                s' -> w : wordsBy p s''
                  where (w, s'') = break p s'

sumRange :: Int -> Int
sumRange n
  | n < 0     = sumRange $ abs n
  | odd n     = sumRange (n - 1) + n
  | otherwise = (1 + n) * (n `div` 2)

solve :: [Int] -> Int
solve points =
  let m = mean points
  in minimum [ sum $ map (\v -> sumRange $ m - v) points
             , sum $ map (\v -> sumRange $ m + 1 - v) points
             ]

mean :: [Int] -> Int
mean xs = floor $ (fromIntegral $ sum xs) / (fromIntegral $ length xs)

main :: IO ()
main = do
  input <- map read . wordsBy (== ',') <$> getContents
  let answer = solve input
  print answer
