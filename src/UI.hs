module UI
    ( buildUI
    ) where

import Control.Lens
import Data.Maybe
import Game.Chess
import Monomer hiding (Color)
import Monomer.Checkerboard
import Monomer.Dragboard
import TextShow

import Model

buildUI :: UIBuilder AppModel AppEvent
buildUI _ model = tree where
    tree = hstack'
        [ zstack
            [ vstack'
                [ box' $ chessBoard `styleBasic`
                    [ sizeReqW $ fixedSize 400
                    , sizeReqH $ fixedSize 400
                    ]
                , separatorLine
                , if model ^. showEditMenu
                    then box_ [alignRight] editButton
                    else zstack
                        [ label gameTurnText
                        , box_ [alignRight] editButton
                        ]
                ]
            , widgetIf (model ^. showPromotionMenu) $
                alert (AppSetPromotionMenu False) promotionMenu
            , widgetIf (not $ null $ model ^. errorMessage) $
                alertMsg (fromMaybe "" $ model ^. errorMessage) $
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
            , if model ^. showEditMenu
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
        , hstack'
            [ turnPanel
            , separatorLine
            , castlePanel
            ]
        , separatorLine
        , label "Drag pieces to put them on the board:"
        , box' $ extraBoard `styleBasic`
            [ sizeReqW $ fixedSize 300
            , sizeReqH $ fixedSize 100
            ]
        ]
    turnPanel = vstack'
        [ labeledRadio' "White's turn" White $ fenData . fenTurn
        , labeledRadio' "Black's turn" Black $ fenData . fenTurn
        ]
    castlePanel = hgrid'
        [ vstack'
            [ label "White:"
            , labeledCheckbox' "O-O" $ fenData . fenCastleWK
            , labeledCheckbox' "O-O-O" $ fenData . fenCastleWQ
            ]
        , vstack'
            [ label "Black:"
            , labeledCheckbox' "O-O" $ fenData . fenCastleBK
            , labeledCheckbox' "O-O-O" $ fenData . fenCastleBQ
            ]
        ]
    labeledRadio' t v l = labeledRadio_ t v l [onChange updateR]
    labeledCheckbox' t l = labeledCheckbox_ t l [onChange updateC]
    updateR :: Color -> AppEvent
    updateR = const AppUpdateFEN
    updateC :: Bool -> AppEvent
    updateC = const AppUpdateFEN
    gameControlPanel = vstack'
        [ buttonPanel
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
            [ label $ "MCTS runs: " <> (showt $ model ^. mctsRuns)
            , hslider_ mctsRuns 100 30000 [dragRate 1]
            ]
        ]
    buttonPanel = vstack'
        [ resetRotateButtons
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
    resetRotateButtons = hgrid'
        [ button "Reset board" (AppSetPosition startpos)
            `nodeEnabled` not (model ^. calculatingResponse)
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
    chessBoard = if model ^. showEditMenu
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
        ] []
    pieceWidgetData = WidgetValue chessPieces
    checkerPath = getPathOrColor model
    box' x = box_ [alignMiddle, alignCenter] x `styleBasic`
        [ sizeReqW $ fixedSize 400
        ]
    gameTurnText = if noLegalMoves
        then "No legal moves"
        else if color (model ^. chessPosition) == White
            then "White's turn"
            else "Black's turn"
    editButton = (if model ^. showEditMenu
        then button "Go back" $ AppSetEditMenu False
        else button "Edit position" $ AppSetEditMenu True)
            `nodeEnabled` not (model ^. calculatingResponse)
    noLegalMoves = null $ legalPlies $ model ^. chessPosition
    minimaxEvaluationText = if null eval
        then "..."
        else showt $ fromJust eval
    eval = model ^. minimaxEvaluation
