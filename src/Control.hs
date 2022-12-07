module Control where

import Brick hiding (Result)
import qualified Graphics.Vty as V
import qualified Brick.Types as T

import Model
import Model.Board
import Control.Monad.IO.Class (MonadIO(liftIO))
import Model.Player
import qualified Model.Board as Board
-- import Model.Player 

-------------------------------------------------------------------------------

control :: PlayState -> BrickEvent n Tick -> EventM n (Next PlayState)
control s ev = case ev of 
  -- AppEvent Tick                   -> nextS s =<< liftIO (play O s)
  T.VtyEvent (V.EvKey V.KEnter _)      -> nextS s { psPass = 0 } =<< liftIO (play s)
  T.VtyEvent (V.EvKey (V.KChar 'p') _) -> nextS s { psPass' = psPass s, psPass = psPass s + 1 } (pass s)
  T.VtyEvent (V.EvKey (V.KChar 'r') _) -> nextS s $ resign s
  T.VtyEvent (V.EvKey V.KUp   _)       -> Brick.continue (move up    s {psKo = False})
  T.VtyEvent (V.EvKey V.KDown _)       -> Brick.continue (move down  s {psKo = False})
  T.VtyEvent (V.EvKey V.KLeft _)       -> Brick.continue (move left  s {psKo = False})
  T.VtyEvent (V.EvKey V.KRight _)      -> Brick.continue (move right s {psKo = False})
  T.VtyEvent (V.EvKey V.KEsc _)        -> Brick.halt s
  _                                    -> Brick.continue s -- Brick.halt s

-------------------------------------------------------------------------------
otherPlayer :: PlayState -> BW
otherPlayer s = Board.flipBW (psTurn s)

pass :: PlayState -> Result Board
pass s 
  | psPass s >= 1 = Board.result (psBoard s)
  | otherwise     = Cont (psBoard s)

resign :: PlayState -> Result Board
resign s = Board.Win $ otherPlayer s

move :: (Pos -> Pos) -> PlayState -> PlayState
move f s = s { psPos = f (psPos s) }

play :: PlayState -> IO (Result Board)
play s = put board turn <$> getPos turn s
  where
    board = psBoard s
    turn  = psTurn  s

getPos :: BW -> PlayState -> IO Pos
getPos bw s = getStrategy bw s (psPos s) (psBoard s) bw

getStrategy :: BW -> PlayState -> Strategy 
getStrategy B s = plStrat (psB s)
getStrategy W s = plStrat (psW s)

-------------------------------------------------------------------------------
nextS :: PlayState -> Result Board -> EventM n (Next PlayState)
-------------------------------------------------------------------------------
nextS s b = case next s b of
  Right s' -> continue s'
  Left res -> halt (s { psResult = res }) 
