{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Data.Char

compute1 :: Int -> String -> Int
compute1 !acc (x0 : x1 : xs)
  | x0 == x1  = compute1 (digitToInt x0 + acc) (x1 : xs)
  | otherwise = compute1 acc (x1 : xs)
compute1 acc _  = acc

compute2 :: Int -> String -> String -> Int
compute2 !acc (x : xs) (y : ys)
  | x == y      = compute2 (digitToInt x + acc) xs ys
  | otherwise   = compute2 acc xs ys
compute2 acc _ _ = 2 * acc

main :: IO ()
main = do
  input <- getLine
  print $ compute1 0 (input ++ take 1 input)
  print $ uncurry (compute2 0) (splitAt (length input `div` 2) input)
