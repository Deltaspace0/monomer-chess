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
        , labeledRadio' "White's turn" White $ fenData . fenTurn
        , labeledRadio' "Black's turn" Black $ fenData . fenTurn
        , separatorLine
        , hgrid
            [ box_ [alignLeft] $ vstack'
                [ label "White:"
                , labeledCheckbox' "O-O" $ fenData . fenCastleWK
                , labeledCheckbox' "O-O-O" $ fenData . fenCastleWQ
                ]
            , box_ [alignRight] $ vstack'
                [ label "Black:"
                , labeledCheckbox' "O-O" $ fenData . fenCastleBK
                , labeledCheckbox' "O-O-O" $ fenData . fenCastleBQ
                ]
            ]
        , separatorLine
        , label "Click on piece to put it on the board:"
        , box' $ checkerboard 6 2 chessPieces `styleBasic`
            [ sizeReqW $ fixedSize 300
            , sizeReqH $ fixedSize 100
            ]
        ]
    labeledRadio' t v l = labeledRadio_ t v l [onChange updateR]
    labeledCheckbox' t l = labeledCheckbox_ t l [onChange updateC]
    chessPieces = makeClickPiece AppAddPiece <$>
        [ ("wP", (White, Pawn))
        , ("wN", (White, Knight))
        , ("wB", (White, Bishop))
        , ("wR", (White, Rook))
        , ("wQ", (White, Queen))
        , ("wK", (White, King))
        , ("bP", (Black, Pawn))
        , ("bN", (Black, Knight))
        , ("bB", (Black, Bishop))
        , ("bR", (Black, Rook))
        , ("bQ", (Black, Queen))
        , ("bK", (Black, King))
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
        , onChange updateFenChecker
        ]
    checkerPath = getPathOrColor model
    updateFenChecker :: ([[Piece]], Int, Int) -> AppEvent
    updateFenChecker = const AppUpdateFEN
    updateR :: Color -> AppEvent
    updateR = const AppUpdateFEN
    updateC :: Bool -> AppEvent
    updateC = const AppUpdateFEN
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
