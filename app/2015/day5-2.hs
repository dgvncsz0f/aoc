module Main (main) where

import Data.List
import System.IO

pairs0 :: String -> [String]
pairs0 (x0 : x1 : x2 : xs)
  | x0 == x1 && x1 == x2 = [x0, x1] : pairs0 (x2 : xs)
pairs0 (x0 : x1 : xs)    = [x0, x1] : pairs0 (x1 : xs)
pairs0 _                 = []

pairs1 :: String -> [String]
pairs1 (x0 : x1 : x2 : xs)
  | x0 == x2  = [x0, x0] : pairs1 (x1 : x2 : xs)
  | otherwise = pairs1 (x1 : x2 : xs)
pairs1 _                   = []

dpairs :: [String] -> [String]
dpairs []         = []
dpairs (xs : xss) = case partition (== xs) xss of
                      ([], xss') -> dpairs xss'
                      (_, xss')  -> xs : dpairs xss'

isNice :: String -> Bool
isNice input = not (null $ dpairs $ pairs0 input) && not (null $ pairs1 input)

readInput :: IO [String]
readInput = isEOF >>= \eof ->
  if eof
  then pure []
  else do
    line <- getLine
    (line :) <$> readInput

main :: IO ()
main = do
  input <- readInput
  print $ length $ filter isNice input
