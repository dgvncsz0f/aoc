module Main where

import Data.List
import Data.Maybe
import qualified Data.Map as M

type BitString = String

freqs :: String -> M.Map Char Int
freqs = foldr (\k m -> M.insertWith (+) k 1 m) M.empty

mcv :: M.Map Char Int -> Char
mcv m =
  let zeroCount = fromMaybe 0 $ M.lookup '0' m
      oneCount  = fromMaybe 0 $ M.lookup '1' m
  in case compare zeroCount oneCount of
       EQ -> '1'
       LT -> '1'
       GT -> '0'

lcv :: M.Map Char Int -> Char
lcv = comp . mcv
  where
    comp '0' = '1'
    comp '1' = '0'
    comp _   = undefined

oxygenGenerator :: [BitString] -> BitString
oxygenGenerator = findValue mcv

co2Scrubber :: [BitString] -> BitString
co2Scrubber = findValue lcv

findValue :: (M.Map Char Int -> Char) -> [BitString] -> BitString
findValue cvf = go 0
  where
    go _   []      = undefined
    go _   [value] = value
    go bit input   =
      let msb = cvf $ freqs $ (!! bit) $ transpose $ input
          input' = filter (\elem -> msb == elem !! bit) input
      in go (bit + 1) input'


bin2dec :: BitString -> Int
bin2dec = foldr go 0 . zip [0..] . reverse . map cast
  where
    go (i, b) n = n + b * 2^i

    cast '0' = 0
    cast '1' = 1
    cast _   = undefined

lifeSupport :: [BitString] -> Int
lifeSupport input = bin2dec (oxygenGenerator input) * bin2dec (co2Scrubber input)

main :: IO ()
main = do
  rawInput <- getContents
  let input = lines $ rawInput
      answer = lifeSupport input
  print answer
