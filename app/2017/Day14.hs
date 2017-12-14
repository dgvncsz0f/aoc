{-# LANGUAGE TupleSections #-}

module Day14 where

import           Control.Applicative
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.List
import           Day10P2

rowHash :: String -> Int -> [Int]
rowHash key r = let input = parseInput (key ++ '-' : show r)
                in denseHash $ compute context input

gridHash :: String -> [[Int]]
gridHash key = map (rowHash key) [0..127]

toBits :: Int -> [Bool]
toBits = justify . binary []
  where
    justify bs = let l = max 0 $ 8 - length bs
                 in replicate l False ++ bs

    binary acc 0 = acc
    binary acc n = let (q, r) = n `divMod` 2
                   in binary ((r == 1) : acc) q

bitstring :: [Int] -> [Bool]
bitstring = concatMap toBits

regions :: [[Bool]] -> Int
regions = countRegions 0 . grid
  where
    grid = S.fromList
           . concatMap (\(x, ys) -> map (x, ) ys)
           . zip [0..]
           . map (map fst . filter snd . zip [0..])

    neighbors (x, y) = [ (x + 1, y)
                       , (x - 1, y)
                       , (x, y + 1)
                       , (x, y - 1)
                       ]

    popConnected g q =
      case S.minView q of
        Nothing       -> g
        Just (xy, q') ->
          let q1 = foldr S.insert q' $ filter (`S.member` g) (neighbors xy)
              g1 = S.delete xy g
          in popConnected g1 q1

    countRegions acc g =
      case S.minView g of
        Nothing       -> acc
        Just (xy, g') ->
          countRegions (acc + 1) (popConnected g' (S.singleton xy))

used :: [[Bool]] -> Int
used = sum . map (length . filter id)

main :: IO ()
main = do
  key  <- getLine
  let bitmap = map bitstring $ gridHash key
  print $ used bitmap
  print $ regions bitmap
