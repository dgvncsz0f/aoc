{-# LANGUAGE BangPatterns #-}

module Main (main) where

import           Data.List (foldl')
import qualified Data.IntMap as M
import qualified Data.IntSet as S
import           System.IO

type G = M.IntMap S.IntSet

insert :: G -> Int -> Int -> G
insert g n0 n1 = M.insertWith S.union n0 (S.singleton n1) g

insertAll :: G -> (Int, [Int]) -> G
insertAll g (n, ns) = foldl' (\g -> uncurry (insert g)) g (concatMap (\n1 -> [(n, n1), (n1, n)]) ns)

search :: Int -> G -> S.IntSet
search k = scan (S.singleton k) S.empty
  where
    scan queue seen g =
      case S.minView queue of
        Nothing          -> seen
        Just (k, queue') ->
          let seen' = S.insert k seen
              nodes = M.findWithDefault S.empty k g
          in scan (S.union queue' (S.difference nodes seen')) seen' g

groups :: G -> Int
groups g = go 0 S.empty g $ M.keys g
  where
    go !acc _ _ []         = acc
    go !acc seen g (k : ks)
      | S.member k seen    = go acc seen g ks
      | otherwise          = let s = search k g
                             in go (acc + 1) (S.union s seen) g ks

trim :: String -> String
trim = trimLeading . trimTrailing
  where
    trimLeading = dropWhile (== ' ')

    trimTrailing []         = []
    trimTrailing (' ' : xs) =
      case trimTrailing xs of
        [] -> []
        xs -> ' ' : xs
    trimTrailing (x : xs)   = x : trimTrailing xs

split :: String -> [String]
split s =
  let (l, r) = break (== ',') s
  in case r of
       [] -> filter (/= "") [trim l]
       r  -> trim l : split (drop 1 r)

parse :: String -> (Int, [Int])
parse input =
  let (n, '<' : '-' : '>' : xs) = break (== '<') input
  in (read n, map read $ split xs)

getLines :: IO [String]
getLines = isEOF >>= \eof ->
  if eof
  then pure []
  else do
    line <- getLine
    (line :) <$> getLines

main :: IO ()
main = do
  input <- map parse <$> getLines
  let g = foldl' insertAll M.empty input
  print $ S.size $ search 0 g
  print $ groups g
