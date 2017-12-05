{-# LANGUAGE BangPatterns #-}

module Main (main) where

import qualified Data.IntMap.Strict as M
import           System.IO

jump0 :: Int -> M.IntMap Int -> (Maybe Int, M.IntMap Int)
jump0 = M.updateLookupWithKey (const (Just . succ))

jump1 :: Int -> M.IntMap Int -> (Maybe Int, M.IntMap Int)
jump1 = let f n = if n >= 3 then pred n else succ n
        in M.updateLookupWithKey (const (Just . f))

count :: (Int -> M.IntMap Int -> (Maybe Int, M.IntMap Int))
      -> M.IntMap Int
      -> Int
count jump m = go m 0 0
  where
    go m j !n =
      case jump j m of
        (Nothing, _)  -> n
        (Just j', m') -> go m' (j' + j) (n + 1)

readInput :: IO [Int]
readInput = isEOF >>= \eof ->
  if eof
  then pure []
  else do
    n <- read <$> getLine
    (n :) <$> readInput

main :: IO ()
main = do
  table <- M.fromList . zip [0..] <$> readInput
  print $ count jump0 table
  print $ count jump1 table
