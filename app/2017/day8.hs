{-# LANGUAGE GADTs      #-}
{-# LANGUAGE TypeInType #-}

module Main (main) where

import           Data.Kind
import           Control.Monad.Free
import qualified Data.Map as M
import           System.IO

type Register = String

type Registers = (M.Map String Int, Int)

type Program = Free Instruction ()

data Instruction :: Type -> Type where
  Cond :: (Registers -> Bool) -> next -> Instruction next
  Exec :: (Registers -> Registers) -> next -> Instruction next

new :: Registers
new = (M.empty, 0)

get :: Register -> Registers -> Int
get k = M.findWithDefault 0 k . fst

put :: Register -> (Int -> Int) -> Registers -> Registers
put k f (r, m) =
  let (value, r') = M.insertLookupWithKey (\_ -> const f) k (f 0) r
  in (r', max m $ maybe (f 0) f value)

cond :: Register -> (Int -> Bool) -> Program
cond k p = liftF (Cond (p . get k) ())

exec :: Register -> (Int -> Int) -> Program
exec r code = liftF (Exec (put r code) ())

parseOpcode :: [String] -> Program
parseOpcode [r, "inc", arg, _, _, _, _] = exec r (+ read arg)
parseOpcode [r, "dec", arg, _, _, _, _] = exec r (+ (negate $ read arg))
parseOpcode _                           = error "compile error"

parseCond :: [String] -> Program
parseCond [_, _, _, "if", r, ">", arg]  = cond r (> read arg)
parseCond [_, _, _, "if", r, "<", arg]  = cond r (< read arg)
parseCond [_, _, _, "if", r, "!=", arg] = cond r (/= read arg)
parseCond [_, _, _, "if", r, ">=", arg] = cond r (>= read arg)
parseCond [_, _, _, "if", r, "<=", arg] = cond r (<= read arg)
parseCond [_, _, _, "if", r, "==", arg] = cond r (== read arg)
parseCond _                             = error "compile error"

compile1 :: String -> Program
compile1 input =
  let args = words input
  in parseCond args >> parseOpcode args

compile :: [String] -> Program
compile = mapM_ compile1

eval :: Registers -> Program -> Registers
eval r (Free (Cond p next))
  | p r                     = eval r next
  | otherwise               = eval r (skip1 next)
eval r (Free (Exec f next)) = eval (f r) next
eval r (Pure ())            = r

skip1 :: Program -> Program
skip1 (Free (Exec _ next)) = next
skip1 _                    = error "compile error"

readInput :: IO [String]
readInput = isEOF >>= \eof ->
  if eof
  then pure []
  else do
    line <- getLine
    (line :) <$> readInput

main :: IO ()
main = do
  runtime <- eval new . compile <$> readInput
  print (maximum $ M.elems $ fst runtime)
  print (snd runtime)

instance Functor Instruction where
  fmap f (Cond x next) = Cond x (f next)
  fmap f (Exec x next) = Exec x (f next)
