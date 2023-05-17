{-# LANGUAGE RecordWildCards #-}

module UI
    ( buildUI
    ) where

import Data.Maybe
import Game.Chess
import Monomer hiding (Color)
import Monomer.Checkerboard
import Monomer.Dragboard
import TextShow

import Composites
import Model

buildUI :: UIBuilder AppModel AppEvent
buildUI _ model@(AppModel{..}) = tree where
    tree = hstack'
        [ zstack
            [ vstack'
                [ box' $ chessBoard `styleBasic`
                    [ sizeReqW $ fixedSize 400
                    , sizeReqH $ fixedSize 400
                    ]
                , separatorLine
                , if _amShowEditMenu
                    then box_ [alignRight] editButton
                    else zstack
                        [ label gameTurnText
                        , box_ [alignRight] editButton
                        ]
                ]
            , widgetIf _amShowPromotionMenu $
                alert (AppSetPromotionMenu False) promotionMenu
            , widgetIf (not $ null _amErrorMessage) $
                alertMsg (fromMaybe "" _amErrorMessage) $
                    AppSetErrorMessage Nothing
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
            , if _amShowEditMenu
                then editControlPanel
                else gameControlPanel
            ]
        ] `styleBasic` [padding 16]
    editControlPanel = vstack'
        [ vstack'
            [ resetRotateButtons
            , hgrid'
                [ button "Apply changes" AppApplyEditChanges
                , button "Clear board" AppClearEditBoard
                ]
            ]
        , separatorLine
        , flagPanel fenData AppUpdateFEN
        , separatorLine
        , label "Drag pieces to put them on the board:"
        , box' $ extraBoard `styleBasic`
            [ sizeReqW $ fixedSize 300
            , sizeReqH $ fixedSize 100
            ]
        ]
    gameControlPanel = vstack'
        [ buttonPanel
        , separatorLine
        , labeledCheckbox "Auto promote to queen" autoQueen
        , labeledCheckbox "Auto respond" autoRespond
        , separatorLine
        , responseOptionsPanel
        , separatorLine
        , hgrid'
            [ label $ "Minimax depth: " <> (showt _amMinimaxDepth)
            , hslider_ minimaxDepth 1 20 [dragRate 1]
            ]
        , hgrid'
            [ label $ "MCTS runs: " <> (showt _amMctsRuns)
            , hslider_ mctsRuns 100 30000 [dragRate 1]
            ]
        ]
    buttonPanel = vstack'
        [ resetRotateButtons
        , hgrid'
            [ if _amCalculatingResponse
                then thinkButton
                else button "Play next response" AppPlayNextResponse
                    `nodeEnabled` (not noLegalMoves)
            , button "Undo move" AppUndoMove `nodeEnabled` all not
                [ null _amPreviousPositions
                , _amCalculatingResponse
                ]
            ]
        ]
    resetRotateButtons = hgrid'
        [ button "Reset board" (AppSetPosition startpos)
            `nodeEnabled` not _amCalculatingResponse
        , button "Rotate board" AppRotateBoard
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
    promotionPieces = if isWhiteTurn model
        then makeClickPiece AppPromote <$>
            [ ("wQ", Queen)
            , ("wR", Rook)
            , ("wB", Bishop)
            , ("wN", Knight)
            ]
        else makeClickPiece AppPromote <$>
            [ ("bQ", Queen)
            , ("bR", Rook)
            , ("bB", Bishop)
            , ("bN", Knight)
            ]
    makeClickPiece f (p, e) = box_ [onBtnReleased $ \_ _ -> f e] $
        image_ ("assets/chess-pieces/" <> p <> ".png") [fitEither]
    hstack' = hstack_ [childSpacing_ 16]
    vstack' = vstack_ [childSpacing_ 16]
    hgrid' = hgrid_ [childSpacing_ 16]
    chessBoard = if _amShowEditMenu
        then editBoard
        else gameBoard
    gameBoard = dragboard_ 8 8 boardState checkerPath
        [ checkerConfig [lightColor gray]
        , moveValidator $ validateMove model
        , onChange AppBoardChanged
        ]
    editBoard = dragboard_ 8 8 (fenData . fenBoardState) checkerPath
        [ checkerConfig [lightColor gray, darkColor darkGray]
        , onChange AppEditBoardChanged
        ]
    extraBoard = dragboardD_ 6 2 pieceWidgetData checkerPath
        [ checkerConfig [lightColor gray, darkColor darkGray]
        , moveValidator $ const False
        , dragIdOffset 1000
        , disableClick
        , renderSource
        ] []
    pieceWidgetData = WidgetValue chessPieces
    checkerPath = getPathOrColor model
    box' x = box_ [alignMiddle, alignCenter] x `styleBasic`
        [ sizeReqW $ fixedSize 400
        ]
    gameTurnText = if noLegalMoves
        then "No legal moves"
        else if color _amChessPosition == White
            then "White's turn"
            else "Black's turn"
    editButton = (if _amShowEditMenu
        then button "Go back" $ AppSetEditMenu False
        else button "Edit position" $ AppSetEditMenu True)
            `nodeEnabled` not _amCalculatingResponse
    noLegalMoves = null $ legalPlies _amChessPosition
    minimaxEvaluationText = if null _amMinimaxEvaluation
        then "..."
        else showt $ fromJust _amMinimaxEvaluation
