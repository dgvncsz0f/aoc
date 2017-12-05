module Main (main) where

import System.IO

area :: Int -> Int -> Int -> Int
area l w h = let sides = [l * w, w * h, h * l]
             in 2 * sum sides + minimum sides

ribbon :: Int -> Int -> Int -> Int
ribbon l w h = let perimeters = [l + w, w + h, h + l]
               in 2 * minimum perimeters + l * w * h

readInput :: IO [(Int, Int, Int)]
readInput = isEOF >>= \eof ->
  if eof
  then pure []
  else do
    [l, w, h] <- map read . split [[]] <$> getLine
    ((l, w, h) :) <$> readInput
  where
    split ws []             = reverse $ map reverse ws
    split ws ('x' : xs)     = split ([] : ws) xs
    split (w : ws) (x : xs) = split ((x : w) : ws) xs
    split _ _               = error "invalid input"

main :: IO ()
main = do
  input <- readInput
  print $ sum $ map (\(l, w, h) -> area l w h) input
  print $ sum $ map (\(l, w, h) -> ribbon l w h) input
