{-# LANGUAGE RecordWildCards #-}
module Model where 

import Prelude hiding ((!!))
import qualified Model.Board  as Board
import qualified Model.Score  as Score
import qualified Model.Player as Player

-------------------------------------------------------------------------------
-- | Ticks mark passing of time: a custom event that we constantly stream
-------------------------------------------------------------------------------
data Tick = Tick

-------------------------------------------------------------------------------
-- | Top-level App State ------------------------------------------------------
-------------------------------------------------------------------------------

data State 
  = Intro 
  | Play PlayState 
  | Outro 
  
data PlayState = PS
  { psB      :: Player.Player   -- ^ Black player info
  , psW      :: Player.Player   -- ^ White player info
  , psScore  :: Score.Score     -- ^ current score
  , psBoard  :: Board.Board     -- ^ current board
  , psBoard' :: Board.Board     -- ^ previous state's board
  , psTurn   :: Board.BW        -- ^ whose turn 
  , psPos    :: Board.Pos       -- ^ current cursor
  , psResult :: Board.Result () -- ^ result      
  , psPass   :: Int             -- ^ number of consecutive passes
  , psPass'  :: Int             -- ^ number of consecutive passes, one turn ago
  , psKo     :: Bool            -- ^ whether last turn was blocked due to superko
  } 

init :: Int -> PlayState
init n = PS 
  { psB      = Player.human
  , psW      = Player.human
  , psScore  = Score.init n
  , psBoard  = Board.init
  , psBoard' = Board.init
  , psTurn   = Board.B
  , psPos    = head Board.positions 
  , psResult = Board.Cont ()
  , psPass   = 0
  , psPass'  = 0
  , psKo     = False
  }

isCurr :: PlayState -> Int -> Int -> Bool
isCurr s r c = Board.pRow p == r && Board.pCol p == c
  where 
    p = psPos s 

next :: PlayState -> Board.Result Board.Board -> Either (Board.Result ()) PlayState
next s Board.Retry     = Right s
next s (Board.Cont b') = Right (if psBoard s'' == psBoard' s && psPass' s == psPass s then s {psKo = True } else s'')
  where
    s'' = do
      let s' = s { psBoard' = psBoard s, psBoard = Board.filterBW (Board.flipBW (psTurn s)) b' }
      s' { psBoard = Board.filterBW (psTurn s') (psBoard s'), psTurn  = Board.flipBW (psTurn s), psKo = False }
next s res             = nextBoard s res 

nextBoard :: PlayState -> Board.Result a -> Either (Board.Result ()) PlayState
nextBoard s res = case res' of
                    Board.Win _ -> Left res' 
                    Board.Draw  -> Left res'
                    _           -> Right s' 
  where 
    sc'  = Score.add (psScore s) (Board.boardWinner res) 
    res' = Score.winner sc'
    s'   = s { psScore = sc'                   -- update the score
             , psBoard = mempty                -- clear the board
             , psBoard' = mempty               -- clear the previous board
             , psTurn  = Score.startPlayer sc' -- toggle start player
             , psPass  = 0                     -- clear the # of consecutive passes
             , psPass' = 0                     -- clear the # of consecutive passes, one turn ago
             , psKo    = False
             } 
