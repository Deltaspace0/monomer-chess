module UI
    ( buildUI
    ) where

import Control.Lens
import Game.Chess
import Monomer
import Monomer.Checkerboard
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
                alert (AppSetPromotionMenu False) promotionMenu
            ]
        , separatorLine
        , vstack'
            [ button "Reset board" AppResetBoard
            , button "Rotate board" AppRotateBoard
            , button "Play random move" AppPlayRandomMove
            , button "Undo move" AppUndoMove
            , labeledCheckbox "Auto promote to queen" autoQueen
            , labeledCheckbox "Auto respond randomly" autoRandom
            ]
        ] `styleBasic` [padding 64]
    promotionMenu = vstack'
        [ label "Promote to:"
        , checkerboard 2 2 promotionPieces `styleBasic`
            [ sizeReqW $ fixedSize 100
            , sizeReqH $ fixedSize 100
            ]
        ]
    promotionPieces = makeClickPiece <$> if isWhiteTurn model
        then
            [ ("wQ", Queen)
            , ("wR", Rook)
            , ("wB", Bishop)
            , ("wN", Knight)
            ]
        else
            [ ("bQ", Queen)
            , ("bR", Rook)
            , ("bB", Bishop)
            , ("bN", Knight)
            ]
    makeClickPiece (p, e) = box_ [onClick $ AppPromote e] $
        image_ ("assets/chess-pieces/" <> p <> ".png") [fitEither]
    hstack' = hstack_ [childSpacing_ 64]
    vstack' = vstack_ [childSpacing_ 16]
    gameBoard = dragboard_ 8 8 boardState (getPathOrColor model)
        [ checkerConfig [lightColor gray]
        , moveValidator $ validateMove model
        , onChange AppBoardChanged
        ]
    box' x = box_ [alignMiddle, alignCenter] x `styleBasic`
        [ sizeReqW $ fixedSize 400
        , sizeReqH $ fixedSize 400
        ]
