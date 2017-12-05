module Main (main) where

import qualified Data.Set as S
import           Data.List
import           System.IO

isPassphrase1 :: [String] -> Bool
isPassphrase1 pass = length pass == S.size (S.fromList pass)

isPassphrase2 :: [String] -> Bool
isPassphrase2 pass = length pass == S.size (S.fromList $ map sort pass)

readLines :: IO [String]
readLines = do
  eof <- isEOF
  if eof
    then pure []
    else do
      line <- getLine
      (line :) <$> readLines

main :: IO ()
main = do
  lines <- map words <$> readLines
  print (length $ filter isPassphrase1 lines)
  print (length $ filter isPassphrase2 lines)
