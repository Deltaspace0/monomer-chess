module UI
    ( buildUI
    ) where

import Control.Lens
import Game.Chess
import Monomer
import Monomer.Dragboard

import Model

buildUI :: UIBuilder AppModel AppEvent
buildUI _ model = tree where
    tree = hstack'
        [ zstack
            [ box' $ gameBoard `styleBasic`
                [ sizeReqW $ fixedSize 400
                , sizeReqH $ fixedSize 400
                ]
            , widgetIf (model ^. showPromotionMenu) $
                alert (AppSetPromotionMenu False) $ vstack'
                    [ label "Promote to:"
                    , button "Queen" $ AppPromote Queen
                    , button "Rook" $ AppPromote Rook
                    , button "Bishop" $ AppPromote Bishop
                    , button "Knight" $ AppPromote Knight
                    ]
            ]
        , separatorLine
        , vstack'
            [ button "Reset board" AppResetBoard
            ]
        ] `styleBasic` [padding 64]
    hstack' = hstack_ [childSpacing_ 64]
    vstack' = vstack_ [childSpacing_ 64]
    gameBoard = dragboard_ 8 8 boardState getPathOrColor
        [ checkerConfig [lightColor gray]
        , moveValidator $ validateMove model
        , onChange AppBoardChanged
        ]
    box' x = box_ [alignMiddle, alignCenter] x `styleBasic`
        [ sizeReqW $ fixedSize 400
        , sizeReqH $ fixedSize 400
        ]
