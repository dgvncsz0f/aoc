module Main where

parse :: [String] -> (Int, Int) -> (Int, Int)
parse ["forward", n] (x, y) = (x + read n, y)
parse ["down", n] (x, y)    = (x, y + read n)
parse ["up", n] (x, y)      = (x, y - read n)
parse input _               = error $ "invalid input" ++ (show input)

main :: IO ()
main = do
  rawInput <- getContents
  let input = reverse . map words . lines $ rawInput
      coordinate = foldr parse (0, 0) input
      answer = fst coordinate * snd coordinate
  print answer
