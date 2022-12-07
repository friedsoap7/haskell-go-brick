{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

import Test.Tasty
import Common
import qualified Model.Board as B
import qualified Data.Map as M

main :: IO ()
main = runTests 
  [ probBoard
  ]

probBoard :: Score -> TestTree
probBoard sc = testGroup "GoBoard"
  [ scoreProp sc ("prop_allPiecesRemoved", B.prop_allPiecesRemoved, 3)
  , scoreProp sc ("prop_allEmptyCounted", B.prop_allEmptyCounted, 3)
  , scoreProp sc ("prop_allPiecesCounted", B.prop_allPiecesCounted, 3)
  ]
