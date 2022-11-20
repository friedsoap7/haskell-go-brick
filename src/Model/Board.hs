{-# LANGUAGE DeriveFunctor #-}
module Model.Board 
  ( -- * Types
    Board
  , BW (..)
  , Pos (..)
  , Result (..)

    -- * Board API
  , dim
  , (!)
  , init
  , put
  , positions
  , emptyPositions
  , boardWinner
  , flipXO

    -- * Moves
  , up
  , down
  , left
  , right
  )
  where

import Prelude hiding (init)
import qualified Data.Map as M 

-------------------------------------------------------------------------------
-- | Board --------------------------------------------------------------------
-------------------------------------------------------------------------------

type Board = M.Map Pos BW

data BW 
  = B 
  | W
  deriving (Eq, Show)

data Pos = Pos 
  { pRow :: Int  -- 1 <= pRow <= dim 
  , pCol :: Int  -- 1 <= pCol <= dim
  }
  deriving (Eq, Ord)

(!) :: Board -> Pos -> Maybe BW 
board ! pos = M.lookup pos board

dim :: Int
dim = 3

positions :: [Pos]
positions = [ Pos r c | r <- [1..dim], c <- [1..dim] ] 

emptyPositions :: Board -> [Pos]
emptyPositions board  = [ p | p <- positions, M.notMember p board]

init :: Board
init = M.empty

-------------------------------------------------------------------------------
-- | Playing a Move
-------------------------------------------------------------------------------
                 
data Result a 
  = Draw 
  | Win BW
  | Retry 
  | Cont a
  deriving (Eq, Functor, Show)

put :: Board -> BW -> Pos -> Result Board
put board xo pos = case M.lookup pos board of 
  Just _  -> Retry
  Nothing -> result (M.insert pos xo board) 

result :: Board -> Result Board
result b 
  | isFull b  = Draw
  | wins b B  = Win  B 
  | wins b W  = Win  W
  | otherwise = Cont b

wins :: Board -> BW -> Bool
wins b xo = or [ winsPoss b xo ps | ps <- winPositions ]

winsPoss :: Board -> BW -> [Pos] -> Bool
winsPoss b xo ps = and [ b!p == Just xo | p <- ps ]

winPositions :: [[Pos]]
winPositions = rows ++ cols ++ diags 

rows, cols, diags :: [[Pos]]
rows  = [[Pos r c | c <- [1..dim]] | r <- [1..dim]]
cols  = [[Pos r c | r <- [1..dim]] | c <- [1..dim]]
diags = [[Pos i i | i <- [1..dim]], [Pos i (dim+1-i) | i <- [1..dim]]]

isFull :: Board -> Bool
isFull b = M.size b == dim * dim

-------------------------------------------------------------------------------
-- | Moves 
-------------------------------------------------------------------------------

up :: Pos -> Pos 
up p = p 
  { pRow = max 1 (pRow p - 1) 
  } 

down :: Pos -> Pos
down p = p 
  { pRow = min dim (pRow p + 1) 
  } 

left :: Pos -> Pos 
left p = p 
  { pCol   = max 1 (pCol p - 1) 
  } 

right :: Pos -> Pos 
right p = p 
  { pCol = min dim (pCol p + 1) 
  } 

boardWinner :: Result a -> Maybe BW
boardWinner (Win xo) = Just xo
boardWinner _        = Nothing

flipXO :: BW -> BW
flipXO B = W
flipXO W = B

