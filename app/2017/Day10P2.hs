module Day10P2
  ( context
  , parseInput
  , mkInput
  , compute
  , denseHash
  , toHex
  ) where

import Data.Char
import Data.Bits

data Context = Context Int [Int]
             deriving (Show)

fromList :: [Int] -> Context
fromList xs = Context 0 xs

context :: Context
context = fromList [0..255]

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

denseHash :: Context -> [Int]
denseHash (Context _ xs) = go xs
  where
    go [] = []
    go xs =
      let (l, r) = splitAt 16 xs
      in foldr1 (xor) l : go r

toHexC :: Int -> Char
toHexC = ("0123456789abcdef" !!)

toHexI :: Int -> String
toHexI n = let l = n `shiftR` 4
               r = n .&. 0x0f
           in [toHexC l, toHexC r]

toHex :: [Int] -> String
toHex = concatMap toHexI

parseInput :: String -> [Int]
parseInput = zip [0..]
             . concat
             . replicate 64
             . (++ [17,31,73,47,23])
             . map ord

main :: IO ()
main = do
  input <- parseInput <$> getLine
  print $ toHex $ denseHash $ compute context input
