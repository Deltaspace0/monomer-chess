module UI
    ( buildUI
    ) where

import Monomer
import Monomer.Dragboard

import Model

buildUI :: UIBuilder AppModel AppEvent
buildUI _ model = tree where
    tree = hstack_ [childSpacing_ 64]
        [ box' $ gameBoard `styleBasic`
            [ sizeReqW $ fixedSize 400
            , sizeReqH $ fixedSize 400
            ]
        , separatorLine
        , vstack_ [childSpacing_ 64]
            [ button "Reset board" AppResetBoard
            ]
        ] `styleBasic` [padding 64]
    gameBoard = dragboard_ 8 8 boardState getPathOrColor
        [ checkerConfig [lightColor gray]
        , moveValidator $ validateMove model
        ]
    box' x = box_ [alignMiddle, alignCenter] x `styleBasic`
        [ sizeReqW $ fixedSize 400
        , sizeReqH $ fixedSize 400
        ]
