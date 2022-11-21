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
  , flipBW
  , result

    -- * Moves
  , up
  , down
  , left
  , right
  )
  where

import Prelude hiding (init)
import qualified Data.Map as M
import qualified Data.Set as S

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
  deriving (Eq, Ord,Show)

(!) :: Board -> Pos -> Maybe BW
board ! pos = M.lookup pos board

dim :: Int
dim = 5

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
put board bw pos = case M.lookup pos board of
  Just _  -> Retry
  Nothing -> Cont (M.insert pos bw board)

result :: Board -> Result Board
result b
  | wins b B  = Win  B
  | wins b W  = Win  W
  | otherwise = Draw

wins :: Board -> BW -> Bool
wins b bw = let
  score = countE b +++ countBW b
   in case bw of
    B -> uncurry (>) score
    W -> uncurry (<) score

countE :: Board -> (Int, Int)
countE b = foldl f (0,0) positions
  where
    f (countB, countW) startPos = (countB + fromEnum foundB, countW + fromEnum foundW)
      where
        (_, foundB, foundW) = explore b startPos S.empty

countBW :: Board -> (Int, Int)
countBW = M.foldlWithKey f (0,0)
  where
    f (countB, countW) _ v = (countB, countW) +++ (fromEnum (v == B), fromEnum (v == W))

explore :: Board -> Pos -> S.Set Pos -> (S.Set Pos, Bool, Bool)
explore b p v = (recVisited, recFoundB || found B, recFoundW || found W)
  where
    found bw = any (g bw) (neighborsOf p)
    g bw pos = b ! pos == Just bw
    (recVisited, recFoundB, recFoundW) = S.foldl h (visited, False, False) emptyNeighbors
      where
        emptyNeighbors = S.fromList $ filter f (neighborsOf p)
          where
            f pos = pos `M.notMember` b && pos `S.notMember` v
        visited     = v `S.union` emptyNeighbors
        h (sp,b1,b2) neigh = (sp `S.union` sp', b1 || b1', b2 || b2')
          where
            (sp',b1',b2') = explore b neigh visited

neighborsOf :: Pos -> [Pos]
neighborsOf pos = map f directions
  where
    f dir = dir pos
    directions = [left, right, up, down]

(+++) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
(+++) (x1, x2) (y1, y2) = (x1 + y1, x2 + y2)

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
boardWinner (Win bw) = Just bw
boardWinner _        = Nothing

flipBW :: BW -> BW
flipBW B = W
flipBW W = B

