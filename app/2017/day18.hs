{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import           Data.List
import           Data.Maybe
import qualified Data.Sequence as S
import qualified Data.IntMap.Strict as M

type Code = M.IntMap Instruction

type Data = M.IntMap Int

type IP = Int

data Runtime = R !IP (S.Seq Int) Data
             | W Register !IP (S.Seq Int) Data
             | D (S.Seq Int) Data
             deriving (Show)

data PRuntime = PRuntime Runtime Runtime !Int !Int
              deriving (Show)

type Register = Int

data Value = Ptr Register
           | Const Int
           deriving (Show)

data Instruction = Snd Value
                 | Rcv Register
                 | Set Register Value
                 | Add Register Value
                 | Mul Register Value
                 | Mod Register Value
                 | Jgz Value Value
                 deriving (Show)

preg :: Register
preg = fromJust $ regi "p"

newRuntime :: Runtime
newRuntime = R 0 S.empty M.empty

newPRuntime :: PRuntime
newPRuntime =
  PRuntime
  (R 0 S.empty (M.singleton preg 0))
  (R 0 S.empty (M.singleton preg 1))
  0 0

fromPtr :: Value -> Int
fromPtr (Ptr i) = i
fromPtr _       = error "bad value"

restart :: Runtime -> Runtime
restart (W r ip mbox mem) = R (ip + 1) mbox mem
restart r                 = r

recv :: Int -> Runtime -> Runtime
recv v (W r ip xs mem) = W r ip xs (store r v mem)
recv _ _               = error "bad state"

send :: Runtime -> (Int, Runtime)
send (W r ip (S.viewl -> (x S.:< xs)) mem) = (x, W r ip xs mem)
send (D (S.viewl -> (x S.:< xs)) mem)      = (x, D xs mem)
send _                                     = error "bad state"

valueof :: Data -> Value -> Int
valueof _ (Const v) = v
valueof m (Ptr i)   = M.findWithDefault 0 i m

store :: Register -> Int -> Data -> Data
store = M.insert

execOp :: (Int -> Int -> Int) -> Register -> Value -> Data -> Data
execOp op r v mem = let v' = valueof mem v
                    in M.alter (Just . maybe (op 0 v') (`op` v')) r mem

execp :: Code -> PRuntime -> PRuntime
execp code (PRuntime p0 p1 cnt0 cnt1) = transfer (exec1 code p0) (exec1 code p1)
  where
    transfer p0@W{} p1@W{}
      | not (null (mbox p0) || null (mbox p1))
        = let (x, p0') = send p0
              (y, p1') = send p1
          in execp code $ PRuntime (restart $ recv y p0') (restart $ recv x p1') (cnt0 + 1) (cnt1 + 1)
      | not (null $ mbox p0)
        = let (x, p0') = send p0
          in execp code $ PRuntime p0' (restart $ recv x p1) (cnt0 + 1) cnt1
      | not (null $ mbox p1)
        = let (x, p1') = send p1
          in execp code $ PRuntime (restart $ recv x p0) p1' cnt0 (cnt1 + 1)
      | otherwise
        = PRuntime p0 p1 (cnt0 + length (mbox p0)) (cnt1 + length (mbox p1))

    transfer p0 p1@W{}
      | not $ null $ mbox p0
        = let (x, p0') = send p0
          in execp code $ PRuntime p0' (restart $ recv x p1) cnt0 (cnt1 + 1)
      | otherwise
        = PRuntime p0 p1 (cnt0 + length (mbox p0)) (cnt1 + length (mbox p1))

    transfer p0@W{} p1
      | not $ null $ mbox p1
        = let (x, p1') = send p1
          in execp code $ PRuntime (restart $ recv x p0) p1' (cnt0 + 1) cnt1
      | otherwise
        = PRuntime p0 p1 (cnt0 + length (mbox p0)) (cnt1 + length (mbox p1))

    transfer p0 p1 = PRuntime p0 p1 (cnt0 + length (mbox p0)) (cnt1 + length (mbox p1))

exec1 :: Code -> Runtime -> Runtime
exec1 code r@D{}           = r
exec1 code r@W{}           = r
exec1 code (R ip mbox mem) =
  case M.lookup ip code of
    Nothing        -> D mbox mem
    Just (Snd a)   ->
      exec1 code $ R (ip + 1) (mbox S.|> valueof mem a) mem
    Just (Set a b) ->
      exec1 code $ R (ip + 1) mbox (store a (valueof mem b) mem)
    Just (Add a b) ->
      exec1 code $ R (ip + 1) mbox (execOp (+) a b mem)
    Just (Mul a b) ->
      exec1 code $ R (ip + 1) mbox (execOp (*) a b mem)
    Just (Mod a b) ->
      exec1 code $ R (ip + 1) mbox (execOp mod a b mem)
    Just (Rcv a)   -> W a ip mbox mem
    Just (Jgz a b) ->
      if valueof mem a > 0
      then exec1 code $ R (ip + valueof mem b) mbox mem
      else exec1 code $ R (ip + 1) mbox mem

part1 :: Code -> Runtime -> Int
part1 code runtime =
  case exec1 code runtime of
    i@(W r _ _ m)
      -> case valueof m (Ptr r) of
           0 -> part1 code $ restart i
           _ -> headr (mbox i)
    _ -> error "bad input"


part2 :: Code -> PRuntime -> Int
part2 code pruntime =
  let PRuntime _ _ _ cnt1 = execp code pruntime
  in cnt1

headr :: S.Seq a -> a
headr xs = case S.viewr xs of
             _ S.:> x -> x
             _        -> error "headl: empty sequence"

regi :: String -> Maybe Int
regi [x] = elemIndex x ['a' .. 'z']
regi _   = Nothing

compile :: String -> Code
compile = M.fromList . zip [0..] . map (parseI . words) . lines

parseI :: [String] -> Instruction
parseI input@("set" : _) = parse2 input (Set . fromPtr)
parseI input@("add" : _) = parse2 input (Add . fromPtr)
parseI input@("snd" : _) = parse1 input Snd
parseI input@("rcv" : _) = parse1 input (Rcv . fromPtr)
parseI input@("mul" : _) = parse2 input (Mul . fromPtr)
parseI input@("mod" : _) = parse2 input (Mod . fromPtr)
parseI input@("jgz" : _) = parse2 input Jgz
parseI _                 = error "bad input"

parse2 :: [String] -> (Value -> Value -> Instruction) -> Instruction
parse2 [_, v0, v1] f = f (maybe (Const $ read v0) Ptr $ regi v0) (maybe (Const $ read v1) Ptr $ regi v1)
parse2 _ _           = error "bad input"

parse1 :: [String] -> (Value -> Instruction) -> Instruction
parse1 [_, v] f = f $ maybe (Const $ read v) Ptr $ regi v
parse1 _ _      = error "bad input"

mbox :: Runtime -> S.Seq Int
mbox (R _ v _)   = v
mbox (W _ _ v _) = v
mbox (D v _)     = v

main :: IO ()
main = do
  code <- compile <$> getContents
  print $ part1 code newRuntime
  print $ part2 code newPRuntime
