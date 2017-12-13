module Main (main) where

import System.IO

type Firewall = [(Int, Int, Int -> Int)]

buildFirewall :: [(Int, Int)] -> Firewall
buildFirewall = map (\(t, d) -> (t, d, at d))
  where
    at d t = let m = 2 * d - 2
                 r = t `mod` m
             in if r >= d
                then m - r
                else r

delay :: Firewall -> Int
delay firewall = search 0
  where
    pass _ []                    = True
    pass delay ((t, _, at) : xs) =
      at (t + delay) /= 0 && pass delay xs

    search amount =
      if pass amount firewall
      then amount
      else search (amount + 1)

severity :: Firewall -> Int
severity []                = 0
severity ((t, d, at) : xs) = if at t == 0
                             then t * d + severity xs
                             else severity xs

parse :: String -> (Int, Int)
parse input =
  let (t, d) = break (== ':') input
  in (read t, read $ drop 2 d)

getLines :: IO [String]
getLines = isEOF >>= \eof ->
  if eof
  then pure []
  else do
    line <- getLine
    (line :) <$> getLines

main :: IO ()
main = do
  input <- buildFirewall . map parse <$> getLines
  print $ severity input
  print $ delay input
