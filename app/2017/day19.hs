module Main where

import qualified Data.Map.Strict as M

import Debug.Trace

type Grid = M.Map (Int, Int) Pipe

data Direction = U | D | L | R
               deriving (Eq, Show)

data Pipe = V | H | X | C Char
          deriving (Show)

parsePipe :: Char -> Maybe Pipe
parsePipe '|'            = Just V
parsePipe '-'            = Just H
parsePipe '+'            = Just X
parsePipe ' '            = Nothing
parsePipe l
  | l >= 'A' && l <= 'Z' = Just $ C l
  | otherwise            = error "bad input"

example = [ "     |          "
          , "     |  +--+    "
          , "     A  |  C    "
          , " F---|----E|--+ "
          , "     |  |  |  D "
          , "     +B-+  +--+ "
          ]

parse :: [String] -> Grid
parse = M.fromList
       . concatMap (\(i, s) -> toPipe i 0 s)
       . zip [0..]
  where
    toPipe _ _ []       = []
    toPipe i j (x : xs) =
      case parsePipe x of
        Nothing -> toPipe i (j + 1) xs
        Just c  -> ((i, j), c) : toPipe i (j + 1) xs

move :: (Int, Int) -> Direction -> (Int, Int)
move (i, j) D = (i + 1, j)
move (i, j) U = (i - 1, j)
move (i, j) L = (i, j - 1)
move (i, j) R = (i, j + 1)

follow :: Grid -> [(Int, Char)]
follow g = nav 1 (fst $ M.findMin g) D
  where
    nav n (i, j) d =
      case M.lookup (i, j) g of
        Just X
          | d `elem` [U, D] && M.member (i, j + 1) g
            -> nav (n + 1) (i, j + 1) R
          | d `elem` [U, D] && M.member (i, j - 1) g
            -> nav (n + 1) (i, j - 1) L
          | d `elem` [L, R] && M.member (i + 1, j) g
            -> nav (n + 1) (i + 1, j) D
          | d `elem` [L, R] && M.member (i - 1, j) g
            -> nav (n + 1) (i - 1, j) U
        Just (C c)
            -> (n, c) : nav 1 (move (i, j) d) d
        Just _
            -> nav (n + 1) (move (i, j) d) d
        Nothing
            -> []

getInput :: IO [String]
getInput = lines <$> getContents

main :: IO ()
main = do
  input <- parse <$> getInput
  let ans = follow input
  print $ map snd ans
  print $ sum $ map fst ans
