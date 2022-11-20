{-# LANGUAGE RecordWildCards #-}
module Model.Score where

import Model.Board (Result (..), BW (..))

-------------------------------------------------------------------------------
-- | Score --------------------------------------------------------------------
-------------------------------------------------------------------------------

data Score = Score 
  { scMax  :: Int  -- ^ total number of boards
  , scB    :: Int  -- ^ points for player X 
  , scW    :: Int  -- ^ points for player O 
  , scD    :: Int  -- ^ drawn games 
  }
  deriving (Eq, Ord, Show)

init :: Int -> Score
init n = Score n 0 0 0

add :: Score -> Maybe BW -> Score
add sc (Just B) = sc { scB = scB sc + 1 }
add sc (Just W) = sc { scW = scW sc + 1 }
add sc Nothing  = sc { scD = scD sc + 1 }

get :: Score -> BW -> Int
get Score {..} B = scB 
get Score {..} W = scW 

currRound :: Score -> Int
currRound Score {..} = scB + scW + scD + 1

startPlayer :: Score -> BW
startPlayer sc 
  | even (currRound sc) = B
  | otherwise           = W

winner :: Score -> Result () 
winner sc@Score {..}
  | scB > scW + left = Win B
  | scW > scB + left = Win W
  | left == 0        = Draw
  | otherwise        = Cont ()
  where 
    left             = 1 + scMax - currRound sc