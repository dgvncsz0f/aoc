module Main (main) where

import qualified Data.Set as S
import           Data.List

move :: [(Int, Int)] -> Int -> Int -> Int -> Int -> [Int]
move [] _ _ _ _ = []
move ((x, k) : xs) k' q r l
  | k == k'   = q + max (min (r - l) 1) 0 : move xs k' q r l
  | otherwise = let distance = (k - (k' + 1)) `mod` l
                in x + q + max (min (r - distance) 1) 0 : move xs k' q r l

redistribute :: [Int] -> S.Set [Int] -> [[Int]]
redistribute banks seen = go (S.insert banks seen) banks
  where
    lbanks = length banks

    ibanks = zip banks [0..]

    maxbank = foldl' f (head ibanks) ibanks
      where
        f (blocks, k) (blocks', k')
          | blocks == blocks' = if k < k'
                                then (blocks, k)
                                else (blocks', k')
          | otherwise         = if blocks > blocks'
                                then (blocks, k)
                                else (blocks', k')

    go seen banks =
      let (blocks, k) = maxbank
          (q, r)      = blocks `quotRem` lbanks
          banks'      = move ibanks k q r lbanks
      in if S.member banks' seen
         then [banks, banks']
         else banks : redistribute banks' seen

main :: IO ()
main = do
  banks <- map read . words <$> getLine
  let allbanks = redistribute banks S.empty
      lastbank = last allbanks
  print $ pred $ length allbanks
  print $ pred $ length $ dropWhile (/= lastbank) allbanks
