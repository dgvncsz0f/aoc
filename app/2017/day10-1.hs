module Main (main) where

data Context = Context Int [Int]
             deriving (Show)

fromList :: [Int] -> Context
fromList xs = Context 0 xs

slice :: Int -> Context -> Context
slice by (Context offset list) =
  let list'   = uncurry (flip (++)) $ splitAt offset list
      (l, r)  = splitAt by list'
      offset' = (-offset) `mod` (length list)
      list''  = uncurry (flip (++)) $ splitAt offset' (reverse l ++ r)
  in Context offset list''

skip :: Int -> Context -> Context
skip len (Context offset list) =
  let offset' = (offset + len) `rem` (length list)
  in Context offset' list

compute :: Context -> [(Int, Int)] -> Context
compute ctx []            = ctx
compute ctx ((s, r) : xs) = compute (skip (s + r) (slice r ctx)) xs

hash :: Context -> Int
hash (Context _ xs) = product $ take 2 xs

split :: String -> [String]
split s =
  let (l, r) = break (== ',') s
  in case r of
       [] -> filter (/= "") [l]
       _  -> l : split (drop 1 r)

example :: [(Int, Int)]
example = zip [0..] [3, 4, 1, 5]

main :: IO ()
main = do
  input <- zip [0..] . map read . split <$> getLine
  print $ hash $ compute (fromList [0..255]) input
