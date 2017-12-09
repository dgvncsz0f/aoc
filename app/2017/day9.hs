{-# LANGUAGE BangPatterns #-}

module Main (main) where

ignore :: Int -> String -> (Int, String)
ignore !c ('>' : xs)     = (c, xs)
ignore !c ('!' : _ : xs) = ignore c xs
ignore !c (_ : xs)       = ignore (c + 1) xs
ignore !c []             = (c, [])

count :: Int -> Int -> Int -> String -> (Int, Int)
count !n !g !r ('{' : xs) = count (n + 1) g r xs
count !n !g !r ('}' : xs) = count (n - 1) (g + n) r xs
count !n !g !r ('<' : xs) = uncurry (count n g) (ignore r xs)
count !n !g !r (',' : xs) = count n g r xs
count _ !g !r []          = (g, r)
count _ _ _ _             = error "bad input"

main :: IO ()
main = do
  input <- getLine
  print $ count 0 0 0 input
