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
  | isFull b  = Draw
  | wins b B  = Win  B
  | wins b W  = Win  W
  | otherwise = Cont b

wins :: Board -> BW -> Bool
wins b bw = let 
  score = countE b `add` countBW b
  add (x,y) (a,b) = (x+a, y+b)
   in case bw of
    B -> uncurry (>) score
    W -> uncurry (<) score
-- wins b xo = or [ winsPoss b xo ps | ps <- winPositions ]

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

countE :: Board -> (Int, Int)
countE b = foldl f (0,0) positions
  where
    f (countB, countW) pos = (countB + fromEnum (sel2 $ res pos), countW + fromEnum (sel3 $ res pos))
    res pos = dfs b pos S.empty

countBW :: Board -> (Int, Int)
countBW = M.foldlWithKey f (0,0)
  where
    f (countB, countW) _ v = if v == B then (countB + 1, countW) else (countB, countW + 1)

sel1 (x, _, _) = x
sel2 (_, y, _) = y
sel3 (_, _, z) = z

dfs :: Board -> Pos -> S.Set Pos -> (S.Set Pos, Bool, Bool)
dfs b p v = (sel1 ress, sel2 ress || foundB, sel3 ress || foundW)
  where
    f pos    = pos `M.notMember` b && pos `S.notMember` v
    g bw pos = M.lookup pos b == Just bw
    neighPos = map (\x -> x p) [left, right, down, up]
    neighEmp = S.fromList $ filter f neighPos
    newV     = v `S.union` neighEmp
    foundB   = any (g B) neighPos
    foundW   = any (g W) neighPos
    res p'   = dfs b p' newV
    ress     = S.foldl h (newV, False, False) neighEmp
      where
        -- h (sp,b1,b2) (sp',b1',b2') = (sp `S.union` sp', b1 || b1', b2 || b2')
        h (sp,b1,b2) x = (sp `S.union` sp', b1 || b1', b2 || b2')
          where
            (sp', b1', b2') = res x


tmp = M.fromList [(Pos 1 1, B), (Pos 1 2, B), (Pos 2 1, B), (Pos 1 3, B), (Pos 3 1, B), (Pos 2 2, B),
                  (Pos 1 4, W), (Pos 2 3, W), (Pos 3 2, W), (Pos 4 1, W)]
-- >>> wins tmp W
-- True
--

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

