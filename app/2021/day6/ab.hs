module Main where

import qualified Data.Map as M

wordsBy :: (Char -> Bool) -> String -> [String]
wordsBy p s = case dropWhile p s of
                "" -> []
                s' -> w : wordsBy p s''
                  where (w, s'') = break p s'

solve :: Int -> [Int] -> Int
solve days seed = go (M.fromListWith (+) $ zip seed (repeat 1)) 0 (length seed)
  where
    go db day total
      | day >= days = total
      | otherwise  =
        case M.lookup (key day) db of
          Nothing    -> go db (day + 1) total
          Just value -> go (nextM db day value) (day + 1) (total + value)

    nextM db day value =
      foldr (\day -> M.insertWith (+) (key day) value)
            (M.delete (key day) db)
            [day + 9, day + 7]

    key day = day `mod` 9

main :: IO ()
main = do
  input <- map read . wordsBy (== ',') <$> getContents
  print $ solve 80 input
  print $ solve 256 input
