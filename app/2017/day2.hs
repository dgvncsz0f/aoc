module Main (main) where

import System.IO
import Data.List

checksum1 :: [Int] -> Int
checksum1 (x : xs) =
  let minmax (maxval, minval) x = (max x maxval, min x minval)
  in uncurry (-) $ foldl' minmax (x, x) xs
checksum1 _        = error "bad input"

checksum2 :: [Int] -> Int
checksum2 (x : xs) =
  let ans = filter (\y -> y > 0 && (max x y `rem` min x y) == 0) xs
  in case ans of
       []  -> checksum2 xs
       [y] -> max x y `quot` min x y
       _   -> error "bad input"
checksum2 _        = error "bad input"

allchecksum :: ([Int] -> Int) -> [[Int]] -> Int
allchecksum f = sum . map f

main :: IO ()
main = do
  table <- getTable
  print $ allchecksum checksum1 table
  print $ allchecksum checksum2 table
  where
    getTable = do
      done <- isEOF
      if done
        then pure []
        else do
          line <- map read . words <$> getLine
          (line :) <$> getTable
