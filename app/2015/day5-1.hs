module Main (main) where

import System.IO

vowels :: String -> String
vowels = filter (`elem` ("aeiou" :: String))

pairs :: String -> [String]
pairs (x0 : x1 : xs) = [x0, x1] : pairs (x1 : xs)
pairs _              = []

blacklist :: [String]
blacklist = ["ab", "cd", "pq", "xy"]

isNice :: String -> Bool
isNice input = let vs = vowels input
                   ps = pairs input
                   ds = filter (\xs -> all (head xs ==) xs) ps
                   bs = filter (`elem` blacklist) ps
               in length vs > 2 && not (null ds) && null bs

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
