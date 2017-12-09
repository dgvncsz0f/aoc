module Main (main) where

import           Data.List
import qualified Data.IntSet as S
import qualified Data.IntMap.Strict as M
import           System.IO

data Mode = On | Off | Toggle

type Grid a = M.IntMap a

type Position = (Int, Int)

grid :: Grid a
grid = M.empty

adjust :: Grid a -> a -> (a -> a) -> Position -> Position -> Grid a
adjust grid z f (x0, _) (x1, _) = let g = Just . maybe (f z) f
                                  in foldl' (flip (M.alter g)) grid [x0..x1]

mode2bool :: Mode -> Position -> Position -> (S.IntSet -> S.IntSet)
mode2bool On (_, y0) (_, y1)     = let mask = S.fromList [y0..y1] in S.union mask
mode2bool Off (_, y0) (_, y1)    = let mask = S.fromList [y0..y1] in (`S.difference` mask)
mode2bool Toggle (_, y0) (_, y1) = let mask = S.fromList [y0..y1]
                                   in \bitmap -> S.union (S.difference mask bitmap) (S.difference bitmap mask)

mode2int :: Mode -> Position -> Position -> (M.IntMap Int -> M.IntMap Int)
mode2int On (_, y0) (_, y1)     = let f m k = M.insertWith (+) k 1 m
                                  in \m -> foldl' f m [y0..y1]
mode2int Off (_, y0) (_, y1)    = let f m k = M.insertWith (\_ b -> max 0 (b - 1)) k 0 m
                                  in \m -> foldl' f m [y0..y1]
mode2int Toggle (_, y0) (_, y1) = let f m k = M.insertWith (+) k 2 m
                                  in \m -> foldl' f m [y0..y1]

count :: Grid S.IntSet -> Int
count = foldl' (\acc bitmap -> S.size bitmap + acc) 0 . M.elems

brightness :: Grid (M.IntMap Int) -> Int
brightness = foldl' (\acc bitmap -> sum (M.elems bitmap) + acc) 0 . M.elems

readLines :: IO [String]
readLines = isEOF >>= \eof ->
  if eof
  then pure []
  else do
    line <- getLine
    (line : ) <$> readLines

parsePos :: String -> Position
parsePos input = case break (== ',') input of
                   (x, ',' : y) -> (read x, read y)
                   _            -> error $ "bad input: " ++ input

parse :: String -> (Mode, Position, Position)
parse input = case words input of
                ["turn", "on", xy0, "through", xy1]
                  -> (On, parsePos xy0, parsePos xy1)
                ["turn", "off", xy0, "through", xy1]
                  -> (Off, parsePos xy0, parsePos xy1)
                ["toggle", xy0, "through", xy1]
                  -> (Toggle, parsePos xy0, parsePos xy1)
                _ -> error $ "bad input: " ++ input

main :: IO ()
main = do
  let f acc (m, p0, p1) = adjust acc S.empty (mode2bool m p0 p1) p0 p1
      g acc (m, p0, p1) = adjust acc M.empty (mode2int m p0 p1) p0 p1
  input <- map parse <$> readLines
  print $ count $ foldl' f grid input
  print $ brightness $ foldl' g grid input
