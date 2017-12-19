{-# LANGUAGE BangPatterns #-}

module Main where

import           Data.List
import           Data.Maybe
import qualified Data.Map.Strict as M

newtype Dance = Dance (M.Map Char Int, M.Map Int Char)
              deriving (Show, Eq)

data Command = Spin Int
             | SwapC Char Char
             | SwapI Int Int
             deriving (Show)

n2i :: Dance -> M.Map Char Int
n2i (Dance val) = fst val

i2n :: Dance -> M.Map Int Char
i2n (Dance val) = snd val

parse :: String -> Command
parse ('x' : input) = let (iA, '/' : iB) = break (== '/') input
                      in SwapI (read iA) (read iB)
parse ('p' : input) = let ([nA], '/' : [nB]) = break (== '/') input
                      in SwapC nA nB
parse ('s' : input) = Spin (read input)
parse _             = error "bad input"

new :: Dance
new = let z = ['a'..'p']
      in Dance (M.fromList (zip z [0..]), M.fromList (zip [0..] z))

spin :: Int -> Dance -> Dance
spin n dance =
  let size = M.size (n2i dance)
  in Dance ( M.map (\i -> (i + n) `mod` size) (n2i dance)
           , M.mapKeys (\i -> (i + n) `mod` size) (i2n dance)
           )

swapI :: Int -> Int -> Dance -> Dance
swapI iA iB dance =
  let nA = fromJust $ M.lookup iA (i2n dance)
      nB = fromJust $ M.lookup iB (i2n dance)
  in swap (iA, nA) (iB, nB) dance

swapC :: Char -> Char -> Dance -> Dance
swapC nA nB dance =
  let iA = fromJust $ M.lookup nA (n2i dance)
      iB = fromJust $ M.lookup nB (n2i dance)
  in swap (iA, nA) (iB, nB) dance

swap :: (Int, Char) -> (Int, Char) -> Dance -> Dance
swap (iA, nA) (iB, nB) dance =
  let n2i' = M.insert nB iA $ M.insert nA iB $ n2i dance
      i2n' = M.insert iA nB $ M.insert iB nA $ i2n dance
  in n2i' `seq` i2n' `seq` Dance (n2i', i2n')

split :: String -> [String]
split s =
  let (l, r) = break (== ',') s
  in case r of
       [] -> [l]
       _  -> l : split (drop 1 r)

exec :: Command -> Dance -> Dance
exec (Spin n)    = spin n
exec (SwapC a b) = swapC a b
exec (SwapI a b) = swapI a b

execLoop :: [Command] -> Int -> Dance -> Dance
execLoop cmds n0 dance0 = at $ enumerate n0 dance0
  where
    at period =
      let size = 1 + length period
      in (dance0 : period) !! (n0 `mod` size)

    enumerate 0 _      = []
    enumerate n !dance =
        let dance' = foldl' (flip exec) dance cmds
        in if dance0 == dance'
           then []
           else dance' : enumerate (pred n) dance'

showDance :: Dance -> String
showDance = map snd . M.toList . i2n

main :: IO ()
main = do
  input <- map parse . split <$> getLine
  print $ showDance $ execLoop input 1 new
  print $ showDance $ execLoop input 1000000000 new
