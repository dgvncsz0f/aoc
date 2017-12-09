{-# LANGUAGE TupleSections #-}

module Main (main) where

import           Control.Applicative
import qualified Data.Map as M
import           Data.List
import           Data.Maybe
import           System.IO

type NodeRef = String

data Node = Node String Int
          deriving (Show, Eq)

data Tree a = Branch a [Tree a]
            | Leaf a
          deriving (Show)

treenode :: Tree a -> a
treenode (Leaf node)     = node
treenode (Branch node _) = node

treebranches :: Tree a -> [Tree a]
treebranches (Leaf _)         = []
treebranches (Branch _ trees) = trees

noderef :: Node -> NodeRef
noderef (Node ref _) = ref

nodeweight :: Node -> Int
nodeweight (Node _ w) = w

updateNode :: Eq a => Tree a -> a -> (Tree a -> Tree a) -> Tree a
updateNode (Leaf root) node treeFn = Branch root [treeFn (Leaf node)]
updateNode (Branch root branches) node treeFn =
  case partition ((== node) . treenode) branches of
    ([tree], branches') -> Branch root (treeFn tree : branches')
    _                   -> Branch root (treeFn (Leaf node) : branches)

insertAll :: Eq a => Tree a -> [a] -> Tree a
insertAll tree []             = tree
insertAll tree (node : nodes) = updateNode tree node (`insertAll` nodes)

trim :: String -> String
trim = trimLeading . trimTrailing
  where
    trimLeading = dropWhile (== ' ')

    trimTrailing []         = []
    trimTrailing (' ' : xs) =
      case trimTrailing xs of
        [] -> []
        xs -> ' ' : xs
    trimTrailing (x : xs)   = x : trimTrailing xs

split :: (Char -> Bool) -> String -> [String]
split p w =
  let (l, r) = break p w
  in case r of
       [] -> filter (/= "") [trim l]
       _  -> trim l : split p (drop 1 r)

fromString :: String -> (Node, [NodeRef])
fromString input =
  let (l, r)   = break (== '-') input
      (n, w)   = (takeWhile (/= ')') . drop 1) <$> break (== '(') l
      nodeRefs = split (== ',') (drop 2 r)
  in (Node (trim n) (read $ trim w), nodeRefs)

fromList :: [(Node, [NodeRef])] -> Maybe (Tree Node)
fromList input = case root of
                   [root'] -> Just $ build (Branch root' []) nodes
                   _       -> Nothing
  where
    nodes = map fst input

    nindex = M.fromList $ map (\node -> (noderef node, node)) nodes

    index = M.fromList $ concatMap (\(node, refs) -> map (, noderef node) refs) input

    root = filter (isNothing . (`M.lookup` index) . noderef) nodes

    parents noderef =
      let nodeof ref = fromJust $ M.lookup ref nindex
      in maybe [] (\p -> nodeof p : parents p) (M.lookup noderef index)

    build tree =
      let path node = drop 1 $ reverse $ node : parents (noderef node)
      in foldl (\tree node -> insertAll tree (path node)) tree

readInput :: IO [String]
readInput = isEOF >>= \eof ->
  if eof
  then pure []
  else do
    line <- getLine
    (line :) <$> readInput

treeweight :: Tree Node -> Tree (Node, Int)
treeweight (Branch node branches) = let weights = map treeweight branches
                                    in Branch (node, nodeweight node + sum (map (snd . treenode) weights)) weights
treeweight (Leaf node)            = Leaf (node, nodeweight node)

findUnbalanced :: Tree (Node, Int) -> Maybe (Tree (Node, Int))
findUnbalanced t@(Branch _ branches) =
  let anyWeight = snd $ treenode $ head branches
  in case partition ((anyWeight /=) . snd . treenode) branches of
       ([tw], _) -> findUnbalanced tw <|> pure t
       (_, [tw]) -> findUnbalanced tw <|> pure t
       _         -> Nothing
findUnbalanced _                     = error "bad input"

weightAdjust :: Tree (Node, Int) -> Int
weightAdjust tree =
  let ws       = map (\tree -> (nodeweight $ fst $ treenode tree, snd $ treenode tree)) (treebranches tree)
      (n0, w0) = head ws
      (n1, w1) = head $ filter ((/= w0) . snd) ws
      n0Adjust = 1 == length (filter ((== w0) . snd) ws)
  in if n0Adjust
     then n0 + (w1 - w0)
     else n1 + (w0 - w1)

main :: IO ()
main = do
  g <- treeweight . fromJust . fromList . map fromString <$> readInput
  print $ noderef $ fst $ treenode g
  print $ weightAdjust $ fromJust $ findUnbalanced g
