{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.Bits
import           Data.Monoid
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.ByteString.Builder
import           Data.Digest.Pure.MD5
import           System.IO
import           System.Environment

digest :: L.ByteString -> Int -> MD5Digest
digest base step = md5 $ toLazyByteString (lazyByteString base <> intDec step)

search :: L.ByteString -> Int -> (MD5Digest -> Bool) -> Either Int Int
search base from p =
  case dropWhile (not . p . fst) digests of
    []           -> Left to
    ((_, n) : _) -> Right n
  where
    to = from + 10000

    digests = map (\n -> (digest base n, n)) [from..to]

zeros5 :: MD5Digest -> Bool
zeros5 md5 = let (b0 : b1 : b2 : _) = B.unpack $ md5DigestBytes md5
             in b0 == 0 && b1 == 0 && b2 .&. 0xF0 == 0

zeros6 :: MD5Digest -> Bool
zeros6 md5 = let (b0 : b1 : b2 : _) = B.unpack $ md5DigestBytes md5
             in b0 == 0 && b1 == 0 && b2 == 0

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  [m, input] <- getArgs
  prog       <- getProgName
  case m of
    "6" -> find (L.pack input) 0 zeros6
    "5" -> find (L.pack input) 0 zeros5
    _   -> error $ "usage: " ++ prog ++ " [5|6] input"
  where
    find input n p =
      case search input n p of
        Left n' -> putStr "." >> find input n' p
        Right n' -> print n'
