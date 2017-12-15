{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Bits

next :: Int -> Int -> Int
next seed initial = (seed * initial) `mod` 2147483647

next4 :: Int -> Int -> Int
next4 seed initial =
  let value = next seed initial
  in if value .&. 0x3 == 0
     then value
     else next4 seed value

next8 :: Int -> Int -> Int
next8 seed initial =
  let value = (seed * initial)  `mod` 2147483647
  in if value .&. 0x7 == 0
     then value
     else next8 seed value

genA :: Int
genA = 16807

genB :: Int
genB = 48271

judge2 :: Int -> Int -> Int
judge2 = count 5000000 0
  where
    count :: Int -> Int -> Int -> Int -> Int
    count 0 !acc _ _       = acc
    count z !acc val0 val1 =
      let val0' = next4 genA val0
          val1' = next8 genB val1
          mask0 = val0' .&. 0xFFFF
          mask1 = val1' .&. 0xFFFF
      in if mask0 == mask1
         then count (pred z) (succ acc) val0' val1'
         else count (pred z) acc val0' val1'

judge1 :: Int -> Int -> Int
judge1 = count 40000000 0
  where
    count :: Int -> Int -> Int -> Int -> Int
    count 0 !acc _ _       = acc
    count z !acc val0 val1 =
      let val0' = next genA val0
          val1' = next genB val1
          mask0 = val0' .&. 0xFFFF
          mask1 = val1' .&. 0xFFFF
      in if mask0 == mask1
         then count (pred z) (succ acc) val0' val1'
         else count (pred z) acc val0' val1'

main :: IO ()
main = do
  seedA <- read . last . words <$> getLine
  seedB <- read . last . words <$> getLine
  print $ judge1 seedA seedB
  print $ judge2 seedA seedB
