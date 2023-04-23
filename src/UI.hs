module UI
    ( buildUI
    ) where

import Control.Lens
import Monomer
import Monomer.Dragboard
import TextShow

import Model

buildUI :: UIBuilder AppModel AppEvent
buildUI _ model = tree where
    tree = hstack_ [childSpacing_ 64]
        [ box' $ gameBoard `styleBasic`
            [ sizeReqW $ fixedSize w
            , sizeReqH $ fixedSize h
            ]
        , separatorLine
        , vstack_ [childSpacing_ 64]
            [ label $ "Rows: " <> (showt r)
            , hslider boardRows 2 12
            , label $ "Cols: " <> (showt c)
            , hslider boardCols 2 12
            , button "Reset board" AppResetBoard
            , toggleButton "All pawns mode" allPawns
            ]
        ] `styleBasic` [padding 64]
    gameBoard = dragboard_ c r boardState (getPathOrColor model)
        [ checkerConfig [lightColor gray]
        , moveValidator validateMove
        ]
    box' x = box_ [alignMiddle, alignCenter] x `styleBasic`
        [ sizeReqW $ fixedSize 400
        , sizeReqH $ fixedSize 400
        ]
    w = if c > r
        then 400
        else 400*(fromIntegral c)/(fromIntegral r)
    h = if c < r
        then 400
        else 400*(fromIntegral r)/(fromIntegral c)
    c = model ^. boardCols
    r = model ^. boardRows
