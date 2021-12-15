{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.Map as M

type LineVent = ((Int, Int), (Int, Int))

parse :: String -> [LineVent]
parse = map (parseLine . words) . lines
  where
    parseLine [ab, "->", cd] = (parsePos ab, parsePos cd)
    parseLine _              = undefined

    parsePos xy =
      let (x, (_ : y)) = break (== ',') xy
      in (read x, read y)


range :: LineVent -> [(Int, Int)]
range ((a, b), (c, d)) = concatMap (\x -> zip (repeat x) (range' b d)) (range' a c)
  where
    range' a b
      | a > b     = reverse [b..a]
      | otherwise = [a..b]

horizontal :: LineVent -> Bool
horizontal ((a, _), (c, _)) = a == c

vertical :: LineVent -> Bool
vertical ((_, b), (_, d)) = b == d

accept :: LineVent -> Bool
accept line = horizontal line || vertical line

overlappingPoints :: [LineVent] -> Int
overlappingPoints lines =
  let grid = M.fromListWith (+) . map (, 1) . concat . map range . filter accept $ lines
  in M.size $ M.filter (> 1) grid

main :: IO ()
main = do
  input <- parse <$> getContents
  let answer = overlappingPoints input
  print answer
