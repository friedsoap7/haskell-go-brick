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
  , filterBW

    -- * Moves
  , up
  , down
  , left
  , right
    -- * Tests
  , prop_allPiecesRemoved
  , prop_allEmptyCounted
  , prop_allPiecesCounted
  )
  where

import Prelude hiding (init)
import qualified Data.Graph as G
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Tree as T
import Data.Maybe ( isNothing, fromMaybe )
import qualified Test.QuickCheck as Q

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
dim = 9

positions :: [Pos]
positions = [ Pos r c | r <- [1..dim], c <- [1..dim] ]

emptyPositions :: Board -> [Pos]
emptyPositions board  = [ p | p <- positions, M.notMember p board]

inBounds :: Pos -> Bool
inBounds p = (pRow p > 0 && pRow p <= dim) && (pCol p > 0 && pCol p <= dim)

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
countE b = foldl f (0,0) $ connectedComponents b
  where
    f (b,w) component = if any (\(_,bw) -> isNothing bw) component
      then
        if all (\(_,bw) -> bw /= Just B) component
          then (b, countEmp + w)
        else if all (\(_,bw) -> bw /= Just W) component
          then (countEmp + b, w)
        else (b,w)
      else (b,w)
      where
        countEmp = length $ filter g component
        g = isNothing . snd

countBW :: Board -> (Int, Int)
countBW = M.foldlWithKey f (0,0)
  where
    f (countB, countW) _ v = (countB, countW) +++ (fromEnum (v == B), fromEnum (v == W))

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

-------------------------------------------------------------------------------
-- Graph-based algorithms
-------------------------------------------------------------------------------

boardAsGraph :: Board -> (G.Graph, G.Vertex -> (Maybe BW, Pos, [Pos]), Pos -> Maybe G.Vertex)
boardAsGraph b = G.graphFromEdges $ map f positions
  where
    f pos   = case b ! pos of
      Nothing -> (Nothing, pos, neighborsOf pos)
      Just bw -> (Just bw, pos, filter (\neigh -> (b ! neigh) /= Just (flipBW bw)) (neighborsOf pos))

connectedComponents :: Board -> [[(Pos, Maybe BW)]]
connectedComponents b = map (map (flipFirst2 . vertexLookup) . T.flatten) (G.components graph)
  where
    flipFirst2 (x, y, _)     = (y, x)
    (graph, vertexLookup, _) = boardAsGraph b

filterBW :: BW -> Board -> Board
filterBW bw b = b `M.withoutKeys` S.fromList (map fst (concat (filter f (connectedComponents b))))
  where
    f comp = all (\bw' -> snd bw' == Just bw) comp


-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

-- generates a board that contains a region surrounded by opposing pieces
genBoardWithCaptures :: Q.Gen (BW, Board)
genBoardWithCaptures = do pos <- Q.elements positions
                          bw <- Q.elements [B,W]
                          (_,b) <- genRec S.empty bw M.empty pos
                          return (bw,b)

-- similar to above but the inner region is empty
genBoardWithoutCaptures :: Q.Gen (BW, S.Set Pos, Board)
genBoardWithoutCaptures = do pos <- Q.elements positions
                             bw <- Q.elements [B,W]
                             (v,b) <- genRec S.empty bw M.empty pos
                             return (bw, v, filterBW bw b)

genRec :: S.Set Pos -> BW -> Board -> Pos -> Q.Gen (S.Set Pos, Board)
genRec v bw m p
      | not (inBounds p) || p `S.member` v  = return (v,m) -- return if p is not in bounds or p is visited
      | otherwise = do bw' <- Q.frequency [(2, return bw), (1, return (flipBW bw))]
                       if bw' == bw then do (v1,b1) <- genRec v' bw m' $ left p
                                            (v2,b2) <- genRec v1 bw b1 $ right p
                                            (v3,b3) <- genRec v2 bw b2 $ up p
                                            (v4,b4) <- genRec v3 bw b3 $ down p
                                            return $ (v4, b4)
                                    else return $ (v', M.insert p bw' m)
      where
        m' = M.insert p bw m
        v' = S.insert p v

genRandomBoard :: Q.Gen Board
genRandomBoard = foldl f (return init) positions
  where
    f g p = do m <- g
               bw <- Q.elements [B, W]
               return $ M.insert p bw m


prop_allPiecesRemoved :: Q.Property
prop_allPiecesRemoved = Q.forAll genBoardWithCaptures (\(bw, b) -> (filterBW bw b) == (M.filter (\v -> v == (flipBW bw)) b))


prop_allEmptyCounted :: Q.Property
prop_allEmptyCounted = Q.forAll genBoardWithoutCaptures (\(bw, _, b) -> ((f bw) (countE b)) == dim * dim - M.size b)
  where
    f bw = if bw == W then fst else snd

prop_allPiecesCounted :: Q.Property
prop_allPiecesCounted = Q.forAll genRandomBoard (\b -> countBW b == (M.size (M.filter f b), M.size (M.filter g b)))
  where
    f = (B ==)
    g = (W ==)
