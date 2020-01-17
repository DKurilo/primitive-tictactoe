module TicTacToe ( Board(..)
                 , Token(..)
                 , Pos(..)
                 , Player(..)
                 , Result(..)
                 , emptyBoard
                 , addToken
                 , winner
                 , nextToken
                 , player
                 , bestMove
                 ) where

import           Control.Monad (join)
import           Data.List     (intercalate, transpose)
import           Data.Ord      (Ord)
import           Debug.Trace

size :: Int
size = 3

size' = size - 1

type Pos = (Int, Int)

data Token = O | X | Empty
    deriving (Eq)

instance Show Token where
    show O     = "O"
    show X     = "X"
    show Empty = "-"

data Result = NotFinished | Won Player | Draw
    deriving (Eq, Show)

instance Ord Result where
    NotFinished <= _ = True
    Won Po <= NotFinished = False
    Won Po <= _ = True
    Draw <= NotFinished = False
    Draw <= Won Po = False
    Draw <= _ = True
    Won Px <= Won Px = True
    Won Px <= _ = False

data Player = Px | Po
    deriving (Eq, Show)

data Tree a = Node a [Tree a] deriving (Show)

newtype Board = MkBoard [[Token]]
    deriving (Eq)

instance Show Board where
    show (MkBoard ts) = (intercalate "\n" . map (intercalate "|" . map show)) ts

emptyBoard :: Board
emptyBoard = MkBoard $ (replicate size . replicate size) Empty

nextToken :: Token -> Token
nextToken X     = O
nextToken O     = X
nextToken Empty = X

player :: Token -> Player
player X = Px
player O = Po
player _ = error "Empty token don't have player"

addToken :: Board -> Token -> Pos -> Maybe Board
addToken b@(MkBoard ts) t (x, y) | x > size' || y > size' = Nothing
                                 | ts !! y !! x == Empty = Just . MkBoard $
                                       take y ts ++ [take x line ++ [t] ++ drop (x + 1) line] ++ drop (y + 1) ts
                                 | otherwise = Nothing
    where line = ts !! y

winner :: Board -> Result
winner b@(MkBoard ts)
    | isWinner X = Won Px
    | isWinner O = Won Po
    | count == size * size = Draw
    | otherwise = NotFinished
    where isWinner t = both ts t || diag ts t || diag (map reverse ts) t || both (transpose ts) t
          both ts' t = any (line t . (ts' !!)) [0..size'] || diag ts' t
          line t = all (==t)
          diag ts' t = and [ts' !! x !! x == t | x <- [0..size']]
          count = sum . map (length . filter (/=Empty)) $ ts


bestMove :: Board -> Token -> Board
bestMove b@(MkBoard ts) t = fst . bestMove $ moves
    where (Node (_, best, _, _) bs) = minimax (gameTree b t)
          moves = map (\(Node (b, _, wx, wo) _) -> if t == X then (b, wx) else (b, wo)) .
                  filter (\(Node (_, r, _, _) _ ) -> r == best) $ bs
          bestMove [] = error "we have no moves!"
          bestMove [(m, x)] = (m, x)
          bestMove ((m, x):ms) = if x >= x' then (m, x) else (m', x')
              where (m', x') = bestMove ms

minimax :: Tree (Board, Token) -> Tree (Board, Result, Int, Int)
minimax (Node (b, _) []) = Node (b, winner b, wx, wo) []
    where (wx, wo) = case winner b of
                         Won Px -> (1, 0)
                         Won Po -> (0, 1)
                         _      -> (0, 0)
minimax (Node (b, t) ns)
    | t == O = Node (b, minimum rs, wx, wo) ns'
    | t == X = Node (b, maximum rs, wx, wo) ns'
    where ns' = map minimax ns
          rs = map (\(Node (_, r, _, _) _) -> r) ns'
          wx = sum . map (\(Node (_, _, wx', _) _) -> wx') $ ns'
          wo = sum . map (\(Node (_, _, _, wo') _) -> wo') $ ns'

gameTree :: Board -> Token -> Tree (Board, Token)
gameTree b t = Node (b, t) . map (`gameTree` t') $ moves b t
    where t' = nextToken t

moves :: Board -> Token -> [Board]
moves b@(MkBoard ts) t
    | winner b == NotFinished = foldr (\p bs -> case addToken b t p of
                                                    Just b' -> b':bs
                                                    _       -> bs) [] [(x, y) | x <- [0..size'], y <- [0..size']]
    | otherwise = []
