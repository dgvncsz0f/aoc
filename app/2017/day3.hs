module Main (main) where

import qualified Data.Map as M
import           Data.List

data Direction = R | L | U | D
               deriving (Show, Eq)

findRank :: Int -> Int
findRank = go 1
  where
    go rank n
      | n <= rank * rank = rank
      | otherwise        = go (rank + 2) n

borders :: Int -> (Int, Int, Int, Int)
borders 1 = (1, 1, 1, 1)
borders n =
  let n2 = n * n
  in ( n2 - 3 * n + 3
     , n2 - 2 * n + 2
     , n2 - n + 1
     , n2
     )

middle  :: Int -> (Int, Int, Int, Int)
middle 1 = (1, 1, 1, 1)
middle n =
  let (x0, x1, x2, x3) = borders n
      halve            = (`div` 2)
  in ( x2 + halve (x3 - x2)
     , x1 + halve (x2 - x1)
     , x0 + halve (x1 - x0)
     , x0 - halve n
     )

distance :: Int -> Int
distance 1 = 1
distance n =
  let rank             = findRank n
      (m0, m1, m2, m3) = middle rank
  in rank `div` 2 + minimum (map (abs . (n -)) [m3, m2, m1, m0])

directions :: [Direction]
directions = R : concatMap moves [3, 5..]
  where
    moves n =
      replicate (n - 2) U
      ++ replicate (n - 1) L
      ++ replicate (n - 1) D
      ++ replicate n R

nearby :: (Int, Int) -> [(Int, Int)]
nearby (y, x) =
  [ (y, x + 1)
  , (y + 1, x + 1)
  , (y + 1, x)
  , (y + 1, x - 1)
  , (y, x - 1)
  , (y - 1, x - 1)
  , (y - 1, x)
  , (y - 1, x + 1)
  ]

nextPos :: Direction -> (Int, Int) -> (Int, Int)
nextPos R (y, x) = (y, x + 1)
nextPos U (y, x) = (y + 1, x)
nextPos L (y, x) = (y, x - 1)
nextPos D (y, x) = (y - 1, x)

valueAt :: (Int, Int) -> M.Map (Int, Int) Int -> (M.Map (Int, Int) Int, Int)
valueAt yx m =
  let value = foldl' (\acc k -> acc + M.findWithDefault 0 k m) 0 (nearby yx)
  in (M.insert yx value m, value)

produce :: [Int]
produce = 1 : go (M.singleton (0, 0) 1) directions (0, 0)
  where
    go m (dir : dirs) yx =
      let yx'     = nextPos dir yx
          (m', v) = valueAt yx' m
      in v : go m' dirs yx'

main :: IO ()
main = do
  input <- read <$> getLine
  print $ distance input
  print $ head $ take 1 $ dropWhile (<= input) produce
