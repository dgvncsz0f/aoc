module Main where

parse :: [String] -> (Int, Int, Int) -> (Int, Int, Int)
parse ["forward", n] (x, y, aim) = (x + read n, y + aim * read n, aim)
parse ["down", n] (x, y, aim)    = (x, y, aim + read n)
parse ["up", n] (x, y, aim)      = (x, y, aim - read n)
parse input _                    = error $ "invalid input" ++ (show input)

main :: IO ()
main = do
  rawInput <- getContents
  let input = reverse . map words . lines $ rawInput
      (x, y, _) = foldr parse (0, 0, 0) input
      answer = x * y
  print answer
