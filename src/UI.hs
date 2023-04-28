module UI
    ( buildUI
    ) where

import Control.Lens
import Data.Maybe
import Game.Chess
import Monomer
import Monomer.Checkerboard
import Monomer.Dragboard
import TextShow

import Model

buildUI :: UIBuilder AppModel AppEvent
buildUI _ model = tree where
    tree = hstack'
        [ zstack
            [ vstack'
                [ box' $ gameBoard `styleBasic`
                    [ sizeReqW $ fixedSize 400
                    , sizeReqH $ fixedSize 400
                    ]
                , separatorLine
                , label gameTurnText
                ]
            , widgetIf (model ^. showPromotionMenu) $
                alert (AppSetPromotionMenu False) promotionMenu
            ]
        , separatorLine
        , vstack'
            [ hstack'
                [ label "Moves:"
                , textField_ sanMoves [readOnly]
                ]
            , hstack'
                [ label "FEN:"
                , textField forsythEdwards
                , button "Load" AppLoadFEN
                ]
            , separatorLine
            , buttonPanel
            , separatorLine
            , labeledCheckbox "Auto promote to queen" autoQueen
            , labeledCheckbox "Auto respond" autoRespond
            , separatorLine
            , responseOptionsPanel
            , separatorLine
            , hgrid'
                [ label $ "Minimax depth: " <>
                    (showt $ model ^. minimaxDepth)
                , hslider_ minimaxDepth 1 20 [dragRate 1]
                ]
            , hgrid'
                [ label $ "MCTS runs: " <>
                    (showt $ model ^. mctsRuns)
                , hslider_ mctsRuns 100 30000 [dragRate 1]
                ]
            ]
        ] `styleBasic` [padding 16]
    buttonPanel = vstack'
        [ hgrid'
            [ button "Reset board" AppResetBoard `nodeEnabled`
                not (model ^. calculatingResponse)
            , button "Rotate board" AppRotateBoard
            ]
        , hgrid'
            [ if model ^. calculatingResponse
                then button (model ^. thinkingAnimation) AppInit
                    `nodeEnabled` False
                else button "Play next response" AppPlayNextResponse
                    `nodeEnabled` (not noLegalMoves)
            , button "Undo move" AppUndoMove `nodeEnabled` all not
                [ null $ model ^. previousPositions
                , model ^. calculatingResponse
                ]
            ]
        ]
    responseOptionsPanel = vstack'
        [ label "How to calculate next response:"
        , labeledRadio "Random" RandomResponse responseMethod
        , hstack'
            [ labeledRadio "Minimax" MinimaxResponse responseMethod
            , label $ "Evaluation: " <> minimaxEvaluationText
            ]
        , labeledRadio "MCTS" MCTSResponse responseMethod
        ]
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
    hstack' = hstack_ [childSpacing_ 16]
    vstack' = vstack_ [childSpacing_ 16]
    hgrid' = hgrid_ [childSpacing_ 16]
    gameBoard = dragboard_ 8 8 boardState (getPathOrColor model)
        [ checkerConfig [lightColor gray]
        , moveValidator $ validateMove model
        , onChange AppBoardChanged
        ]
    box' x = box_ [alignMiddle, alignCenter] x `styleBasic`
        [ sizeReqW $ fixedSize 400
        , sizeReqH $ fixedSize 400
        ]
    gameTurnText = if noLegalMoves
        then "No legal moves"
        else if color (model ^. chessPosition) == White
            then "White's turn"
            else "Black's turn"
    noLegalMoves = null $ legalPlies $ model ^. chessPosition
    minimaxEvaluationText = if null eval
        then "..."
        else showt $ fromJust eval
    eval = model ^. minimaxEvaluation
