module View (view, attributeMap) where

import Brick
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (border, hBorder, vBorder)
import Brick.Widgets.Border.Style (unicode)
import Text.Printf (printf)

import Model
import Model.Board
import Model.Score (currRound, scMax, Score (scW, scB, scD))
import Graphics.Vty hiding (dim)

-------------------------------------------------------------------------------
view :: PlayState -> [Widget String]
-------------------------------------------------------------------------------
view s = [header s <=> view' s]

view' :: PlayState -> Widget String
view' s =
  withBorderStyle unicode $
    -- borderWithLabel (str (header s)) $
      border $
      vTile [ mkRow s row | row <- [1..dim] ]

header :: PlayState -> Widget String
header s =
  vBox  [ str (printf "Go (%dx%d)" dim dim)
        , str (printf "match score | %s: %d | %s: %d | Draws: %d" (bwHumanReadable W ) (scW sc) (bwHumanReadable B) (scB sc) (scD sc))
        , hBox  [ str $ printf "game %d of %d | %s to move" (currRound sc) (scMax sc) (bwHumanReadable $ psTurn s)
                  , str " | "
                  , currentCellStatus s
                ]
        ]
  where
    sc = psScore s

-- format the currently selected cell string w/ contextual information (e.g. already occupied, superko, etc)
-- TODO distinguish between already occupied / disallowed bc of X reason (e.g. superko) / valid
currentCellStatus :: PlayState -> Widget String
currentCellStatus s = case (put board turn p, psKo s) of
  (Retry, _)   -> withAttr redAttr $ str $ currentCellOccupiedStr s
  -- Cont map  -> -- TODO
  (_, True )   -> withAttr redAttr $ str $ currentCellSuperkoStr s
  (_, False)   -> withAttr greenAttr $ str $ currentCellDefaultStr s
  where
    board = psBoard s
    turn = psTurn s
    p = psPos s


currentCellDefaultStr :: PlayState -> String
currentCellDefaultStr s = printf ("currently selected cell: (%d, %d)") (pRow p) (pCol p) -- green
  where
    p = psPos s

currentCellOccupiedStr :: PlayState -> String
currentCellOccupiedStr s = currentCellDefaultStr s ++ " | already occupied" -- red

currentCellSuperkoStr :: PlayState -> String
currentCellSuperkoStr s = currentCellDefaultStr s ++ " | cannot repeat board position" -- red


bwHumanReadable :: BW -> String
bwHumanReadable B = "■ Black"
bwHumanReadable W = "□ White"

mkRow :: PlayState -> Int -> Widget n
mkRow s row = hTile [ mkCell s row i | i <- [1..dim] ]

mkCell :: PlayState -> Int -> Int -> Widget n
mkCell s r c
  | isCurr s r c = withCursor raw
  | otherwise    = raw
  where
    raw = mkCell' s r c

withCursor :: Widget n -> Widget n
withCursor = modifyDefAttr (`withStyle` reverseVideo)

mkCell' :: PlayState -> Int -> Int -> Widget n
-- mkCell' _ r c = center (str (printf "(%d, %d)" r c))
mkCell' s r c = center (mkBW bwMb)
  where
    bwMb      = psBoard s ! Pos r c
    -- xoMb 
    --   | r == c    = Just X 
    --   | r > c     = Just O 
    --   | otherwise = Nothing

mkBW :: Maybe BW -> Widget n
mkBW Nothing  = blockE
mkBW (Just B) = blockB
mkBW (Just W) = blockW

blockE, blockB, blockW :: Widget n
blockE = str " "
blockB = str "■"
blockW = str "□"

vTile :: [Widget n] -> Widget n
vTile (b:bs) = vBox (b : [hBorder <=> b | b <- bs])
vTile _      = emptyWidget

hTile :: [Widget n] -> Widget n
hTile (b:bs) = hBox (b : [vBorder <+> b | b <- bs])
hTile _      = emptyWidget


-- attributes
attributeMap :: AttrMap
attributeMap = attrMap defAttr [
  (redAttr, fg red)
  , (greenAttr, fg green)
  , (blueAttr, fg blue)
  ]

redAttr, greenAttr, blueAttr :: AttrName
redAttr = attrName "redAttr"
greenAttr = attrName "greenAttr"
blueAttr = attrName "blueAttr"