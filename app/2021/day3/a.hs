module Main where

import Data.List
import Data.Function
import qualified Data.Map as M

type BitString = String

freqs :: String -> M.Map Char Int
freqs = foldr (\k m -> M.insertWith (+) k 1 m) M.empty

gamma :: [BitString] -> BitString
gamma = map (maxFreq . freqs) . transpose
  where
    maxFreq = fst . maximumBy (compare `on` snd) . M.toList

epsilon :: [BitString] -> BitString
epsilon = map comp . gamma
  where
    comp '1' = '0'
    comp '0' = '1'
    comp _   = undefined

powerConsumption :: [BitString] -> Int
powerConsumption input = bin2dec (gamma input) * bin2dec (epsilon input)

bin2dec :: BitString -> Int
bin2dec = foldr go 0 . zip [0..] . reverse . map cast
  where
    go (i, b) n = n + b * 2^i

    cast '0' = 0
    cast '1' = 1
    cast _   = undefined

main :: IO ()
main = do
  rawInput <- getContents
  let input = lines $ rawInput
      answer = powerConsumption input
  print answer
