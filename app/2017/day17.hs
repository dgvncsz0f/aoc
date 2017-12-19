{-# LANGUAGE MultiWayIf   #-}

module Main where

newtype Spinlock = Spinlock (Int, Int, Int)
                 deriving (Eq, Show)

spin :: Int -> Spinlock -> Spinlock
spin n (Spinlock (offset, size, _)) =
  let key = 1 + (offset + n) `mod` size
  in Spinlock (key, size + 1, size)

spinList :: Int -> Int -> [Int]
spinList n = toList [] . produce (Spinlock (0, 1, 0))
  where
    produce s 0 = [s]
    produce s r = s : produce (spin n s) (pred r)

    toList acc []                           = acc
    toList acc (Spinlock (k, _, v) : spins) =
      let (l, r) = splitAt k acc
      in toList (l ++ v : r) spins

spinTrack :: Int -> Int -> Int
spinTrack n = go z z
  where
    z = Spinlock (0, 1, 0)

    afterZ (Spinlock (k1, _, _)) = k1 == 1

    go (Spinlock (_, _, v)) _ 0 = v
    go s0 s r                   =
      let s1 = spin n s
      in if | afterZ s1 -> go s1 s1 (pred r)
            | otherwise -> go s0 s1 (pred r)

part1 :: Int -> Int
part1 input =
  let spins = spinList input 2017
  in dropWhile (/= 2017) spins !! 1

part2 :: Int -> Int
part2 input = spinTrack input 50000000

main :: IO ()
main = do
  input <- read <$> getLine
  print $ part1 input
  print $ part2 input
