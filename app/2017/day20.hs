module Main (main) where

import Prelude hiding (subtract)
import Data.List
import Data.Function

data Particle = Particle { partS :: (Int, Int, Int)
                         , partV :: (Int, Int, Int)
                         , partA  :: (Int, Int, Int)
                         }
              deriving (Eq, Show)

norm :: [Particle] -> Int
norm = maximum . map steps
  where
    steps p =
      let (v0, v1, v2) = partV p
          (a0, a1, a2) = partA p
      in 1 + maximum [ if a0 == 0 then 0 else abs v0 `div` abs a0
                     , if a1 == 0 then 0 else abs v1 `div` abs a1
                     , if a2 == 0 then 0 else abs v2 `div` abs a2
                     ]

pdistance :: Int -> Particle -> (Int, Int)
pdistance vsteps p =
  let (v0, v1, v2) = partV p
      (a0, a1, a2) = partA p
      v0'          = v0 + a0 * vsteps
      v1'          = v1 + a1 * vsteps
      v2'          = v2 + a2 * vsteps
  in (distance (a0, a1, a2), distance (v0', v1', v2'))

split :: String -> [String]
split s = let (l, r) = break (== ',') s
          in case r of
               [] -> [l]
               r' -> l : (split $ drop 1 r')

parseTuple :: String -> (Int, Int, Int)
parseTuple input =
  let [i0, i1, i2] = split $ takeWhile (/= '>') $ drop 3 input
  in (read i0, read i1, read i2)

parse :: String -> Particle
parse input =
  let [p, v, a] = map parseTuple $ words input
  in Particle p v a

readInput :: IO [String]
readInput = lines <$> getContents

distance :: (Int, Int, Int) -> Int
distance (a, b, c) = abs a + abs b + abs c

isNat :: Double -> Bool
isNat x = x == fromIntegral (floor x :: Int) && x >= 0

apply :: (a -> a -> a) -> (a, a ,a) -> (a, a, a) -> (a, a, a)
apply f (a0, b0, c0) (a1, b1, c1) = (f a0 a1, f b0 b1, f c0 c1)

roots :: Double -> Double -> Double -> [Double]
roots 0 0 c = [c]
roots 0 b c = filter isNat [-c / b]
roots a b c
  | isNaN d   = []
  | otherwise = filter (\n -> isNat n) [s0, s1]
  where
    s0 = (d - b) / a2
    s1 = (-d - b) / a2
    a2 = a * 2
    d  = sqrt (b * b - 4 * a * c)

proots :: Particle -> [Double]
proots p =
  let (s0, s1, s2) = toDouble $ partS p
      (v0, v1, v2) = toDouble $ partV p
      (a0, a1, a2) = toDouble $ partA p
      roots0          = roots a0 (v0 * 2 + a0) (s0 * 2)
      roots1          = roots a1 (v1 * 2 + a1) (s1 * 2)
      roots2          = roots a2 (v2 * 2 + a2) (s2 * 2)
  in concat [roots0, roots1, roots2]

psolve :: Particle -> Double -> (Double, Double, Double)
psolve p0 t =
  let (s0, s1, s2) = toDouble $ partS p0
      (v0, v1, v2) = toDouble $ partV p0
      (a0, a1, a2) = toDouble $ partA p0
  in ( s0*2 + t * (a0*t + (v0*2 + a0))
     , s1*2 + t * (a1*t + (v1*2 + a1))
     , s2*2 + t * (a2*t + (v2*2 + a2))
     )

toDouble :: (Int, Int, Int) -> (Double, Double, Double)
toDouble (x0, x1, x2) = (fromIntegral x0, fromIntegral x1, fromIntegral x2)

subtract :: Particle -> Particle -> Particle
subtract p0 p1 =
  Particle
  (apply (-) (partS p0) (partS p1))
  (apply (-) (partV p0) (partV p1))
  (apply (-) (partA p0) (partA p1))

collides :: Particle -> Particle -> Bool
collides p0 p1 = any (\r -> psolve p0 r == psolve p1 r) (proots $ subtract p0 p1)

part1 :: [Particle] -> Int
part1 ps = fst $ minimumBy (compare `on` pcmp) (zip [0..] ps)
  where
    pcmp = pdistance (norm ps) . snd

part2 :: [Particle] -> [Particle]
part2 []       = []
part2 (p : ps) =
  case filter (collides p) ps of
    [] -> p : part2 ps
    xs -> part2 (filter (`notElem` xs) ps)

main :: IO ()
main = do
  input <- map parse <$> readInput
  print $ part1 input
  print $ length $ part2 input
