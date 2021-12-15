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
range line@((a, b), (c, d))
  | diagonal line                    = diagRange
  | horizontal line || vertical line = lineRange
  | otherwise                        = undefined
  where
    range' a b
      | a > b     = reverse [b..a]
      | otherwise = [a..b]

    diagRange = zip (range' a c) (range' b d)

    lineRange = concatMap (\x -> zip (repeat x) (range' b d)) (range' a c)

horizontal :: LineVent -> Bool
horizontal ((a, _), (c, _)) = a == c

vertical :: LineVent -> Bool
vertical ((_, b), (_, d)) = b == d

diagonal :: LineVent -> Bool
diagonal ((a, b), (c, d)) = abs (a - c) == abs (b - d)

accept :: LineVent -> Bool
accept line = horizontal line || vertical line || diagonal line

overlappingPoints :: [LineVent] -> Int
overlappingPoints lines =
  let grid = M.fromListWith (+) . map (, 1) . concat . map range . filter accept $ lines
  in M.size $ M.filter (> 1) grid

main :: IO ()
main = do
  input <- parse <$> getContents
  let answer = overlappingPoints input
  print answer
