module Main where

count :: [String] -> Int
count []       = 0
count (x : xs) = case length x of
                   2 -> 1 + count xs
                   3 -> 1 + count xs
                   4 -> 1 + count xs
                   7 -> 1 + count xs
                   _ -> count xs

parse :: String -> [String]
parse input = concatMap parse1 $ lines input
  where
    parse1 input =
      let (_, (_ : right)) = break (== '|') input
      in words right

main :: IO ()
main = do
  input <- parse <$> getContents
  let answer = count input
  print answer


0 = 6
 1 = 2
2 = 5
3 = 5
 4 = 4
5 = 5
6 = 6
 7 = 3
 8 = 7
9 = 6

acedgfb = 8
dab     = 7
ab      = 1
eafb    =

1,4,7

1 + 7         = d
1 + 4 + 7     = c + g
1 + d + c     = f
d + c + f + g = a
a + 1         = b
c + b + f +d  = e
