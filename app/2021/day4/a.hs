module Main where

import Data.List

data Board = Board [[(Int, Bool)]]
  deriving (Show)

data Game = Game { draws  :: [Int]
                 , boards :: [Board]
                 }
  deriving (Show)

wordsBy :: (Char -> Bool) -> String -> [String]
wordsBy p s = case dropWhile p s of
                "" -> []
                s' -> w : wordsBy p s''
                  where (w, s'') = break p s'

parse :: String -> Game
parse input = game
  where
    parseDraws = map read . wordsBy (== ',') . head

    parseBoard input = Board $ map (map (\s -> (read s, False)) . words) input

    parseBoards input =
      case drop 1 input of
        []     -> []
        input' -> (parseBoard $ take 5 input') : (parseBoards $ drop 5 input')

    game =
      let input' = lines input
      in Game (parseDraws $ take 1 input') (parseBoards $ drop 1 input')

draw :: Board -> Int -> Board
draw (Board board) num = Board $ map (map mark') $ board
  where
    mark' (value, checked) = (value, checked || num == value)

isWinner :: Board -> Bool
isWinner (Board board) = check board || check (transpose board)
  where
    check = any (all snd)

game :: Game -> (Int, Board)
game (Game draws boards) = game' boards draws
  where
    game' _ []            = undefined
    game' boards (x : xs) =
      let boards' = map (flip draw x) boards
      in case filter isWinner boards' of
           []           -> game' boards' xs
           (board' : _) -> (x, board')

score :: Board -> Int -> Int
score (Board board) n = n * (sum $ map (sum . map fst . filter (not . snd)) $ board)

main :: IO ()
main = do
  rawInput <- getContents
  let gameState = parse rawInput
      (lastDraw, winner) = game gameState
      score' = score winner lastDraw
  print score'
