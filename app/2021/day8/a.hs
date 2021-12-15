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
