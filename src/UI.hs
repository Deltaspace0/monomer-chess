{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module UI
    ( buildUI
    ) where

import Data.Maybe
import Data.Sequence (Seq(..))
import Game.Chess
import Monomer hiding (Color)
import Monomer.Checkerboard
import Monomer.Dragboard
import TextShow
import qualified Data.Sequence as Seq

import Composites
import Model

buildUI :: UIBuilder AppModel AppEvent
buildUI _ model@(AppModel{..}) = tree where
    tree = keystroke keyShortcuts $ hstack'
        [ zstack
            [ vstack'
                [ box' $ chessBoardLeft `styleBasic`
                    [ sizeReqW $ fixedSize 400
                    , sizeReqH $ fixedSize 400
                    ]
                , separatorLine
                , if _amShowEditMenu
                    then box_ [alignRight] editButton
                    else zstack
                        [ label gameTurnText
                        , box_ [alignRight] $ hstack'
                            [ label currentDepthText
                            , editButton
                            ]
                        ]
                , separatorLine
                , vscroll_ [wheelRate 32] $ vstack' $
                    label <$> _uciPrincipalVariations
                ]
            , widgetIf _amShowPromotionMenu $
                alert (AppSetPromotionMenu False) promotionMenu
            ] `styleBasic` [sizeReqW $ fixedSize 400]
        , separatorLine
        , vstack'
            [ moveHistoryButtons
            , separatorLine
            , moveHistoryPanel
            ]
        , separatorLine
        , zstack
            [ rightPanel
            , widgetIf (not $ null _amErrorMessage) $
                alertMsg (fromMaybe "" _amErrorMessage) $
                    AppSetErrorMessage Nothing
            ]
        ] `styleBasic` [padding 16]
    keyShortcuts = zipWith (,) ["Up", "Left", "Right", "Down"] plyEvents
    moveHistoryButtons = hgrid' $ zipWith ($)
        [ flip nodeEnabled notFirstPosition . button "<<"
        , flip nodeEnabled notFirstPosition . button "<"
        , flip nodeEnabled notLastPosition . button ">"
        , flip nodeEnabled notLastPosition . button ">>"
        ] plyEvents
    plyEvents = AppPlyNumberChanged <$>
        [ 0
        , _amCurrentPlyNumber-1
        , _amCurrentPlyNumber+1
        , Seq.length _amPreviousPositions-1
        ]
    notFirstPosition = _amCurrentPlyNumber > 0
    notLastPosition = _amCurrentPlyNumber < Seq.length _amPreviousPositions-1
    moveHistoryPanel = vscroll_ [wheelRate 32]
        (vstack_ [childSpacing_ 4] (makeHistoryLine <$> moveIndices))
            `styleBasic` [sizeReqW $ fixedSize 204]
    moveIndices = if firstMoveColor == White
        then [0..(Seq.length _amPreviousPositions) `div` 2 - 1]
        else [0..(Seq.length _amPreviousPositions + 1) `div` 2 - 1]
    makeHistoryLine i = hstack
        [ labelS (i+1) `styleBasic` [sizeReqW $ fixedSize 30]
        , hgrid_ [childSpacing_ 4]
            [ if i1 < 1
                then filler
                else optionButton_ t1 i1 currentPlyNumber
                    [onChange AppPlyNumberChanged]
            , if l-i2 < 0
                then filler
                else optionButton_ t2 i2 currentPlyNumber
                    [onChange AppPlyNumberChanged]
            ] `styleBasic` [sizeReqW $ fixedSize 164]
        ] where
            (_, _, _, t1) = Seq.index _amPreviousPositions $ l-i1
            (_, _, _, t2) = Seq.index _amPreviousPositions $ l-i2
            l = Seq.length _amPreviousPositions-1
            (i1, i2) = if firstMoveColor == White
                then (i*2+1, i*2+2)
                else (i*2, i*2+1)
    firstMoveColor = let _ :|> (p, _, _, _) = _amPreviousPositions in color p
    rightPanel = vstack' $ if _amShowTwoBoards
        then
            [ box' $ chessBoardRight `styleBasic`
                [ sizeReqW $ fixedSize 400
                , sizeReqH $ fixedSize 400
                ]
            , separatorLine
            , buttonPanel
            ]
        else
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
    editControlPanel = vstack'
        [ buttonPanel
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
        , vscroll_ [wheelRate 32] $ vstack'
            [ labeledCheckbox' "Rotate board" boardRotated
            , labeledCheckbox' "Auto promote to queen" autoQueen
            , labeledCheckbox' "Auto respond" autoRespond
            , separatorLine
            , aiPanel aiData
            , separatorLine
            , uciPanel
            ] `styleBasic` [padding 8]
        ]
    buttonPanel = vstack' $ if _amShowEditMenu
        then
            [ resetTwoBoardsButtons
            , hgrid'
                [ button "Apply changes" AppApplyEditChanges
                , button "Clear board" AppClearEditBoard
                ]
            ]
        else
            [ resetTwoBoardsButtons
            , hgrid' $ if calculatingResponse
                then
                    [ thinkButton
                    , button "Abort response" AppAbortNextResponse
                    ]
                else
                    [ button "Play next response" AppPlayNextResponse
                        `nodeEnabled` (not noLegalMoves)
                    , button "Undo move" AppUndoMove
                        `nodeEnabled` (length _amPreviousPositions >= 2)
                    ]
            ]
    resetTwoBoardsButtons = hgrid'
        [ button "Reset board" (AppSetPosition startpos)
            `nodeEnabled` not calculatingResponse
        , toggleButton "Two boards" showTwoBoards
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
    labeledCheckbox' t l = labeledCheckbox_ t l [textRight]
    (chessBoardLeft, chessBoardRight) = if _amBoardRotated
        then (chessBoardR, chessBoard)
        else (chessBoard, chessBoardR)
    (chessBoard, chessBoardR) = if _amShowEditMenu
        then (editBoard, editBoardR)
        else (gameBoard, gameBoardR)
    gameBoard = dragboard_ 8 8 boardState checkerPath
        [ checkerConfig [lightColor gray]
        , moveValidator $ validateMove model False
        , onChange $ AppBoardChanged False
        ]
    gameBoardR = dragboard_ 8 8 boardStateReversed checkerPath
        [ checkerConfig [lightColor gray]
        , moveValidator $ validateMove model True
        , onChange $ AppBoardChanged True
        ]
    editBoard = dragboard_ 8 8 (fenData . fenBoardState) checkerPath
        [ checkerConfig [lightColor gray, darkColor darkGray]
        , onChange $ AppEditBoardChanged False
        ]
    editBoardR = dragboard_ 8 8 (fenData . fenBoardStateReversed) checkerPath
        [ checkerConfig [lightColor gray, darkColor darkGray]
        , onChange $ AppEditBoardChanged True
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
    currentDepthText = if null _uciCurrentEngineDepth
        then "No analysis available"
        else "Current depth: " <> fromJust _uciCurrentEngineDepth
    editButton = (if _amShowEditMenu
        then button "Go back" $ AppSetEditMenu False
        else button "Edit position" $ AppSetEditMenu True)
            `nodeEnabled` not calculatingResponse
    noLegalMoves = null $ legalPlies _amChessPosition
    uciPanel = vstack_ [childSpacing_ 16]
        [ label "UCI engine settings"
        , hstack_ [childSpacing_ 16]
            [ label "Path: "
            , textField $ uciData . enginePath
            , if _uciEngineLoading
                then button "Wait" AppLoadEngine `nodeEnabled` False
                else button "Load" AppLoadEngine
            ]
        , if null _uciRequestMVars
            then label "UCI engine is not loaded"
            else hgrid'
                [ button "Send 'stop' command" $ AppSendEngineRequest "stop"
                , button "Halt engine" $ AppSendEngineRequest "eof"
                ]
        , separatorLine
        , zstack_ [onlyTopActive_ False]
            [ labeledCheckbox_ "Record UCI logs to file" uciRecordLogs
                [ textRight
                , onChange AppRecordUCILogsChanged
                ]
            , box_
                [ alignRight
                , ignoreEmptyArea
                ] $ button "Clear" AppClearUciLogs
            ]
        , textArea_ uciLogs [readOnly] `styleBasic` [sizeReqH $ fixedSize 128]
        , separatorLine
        , hgrid'
            [ label $ "Engine depth: " <> (showt _uciEngineDepth)
            , hslider_ (uciData . engineDepth) 1 100 [dragRate 1]
            ]
        , hgrid'
            [ label $ "Number of variations: " <> (showt _uciEngineLines)
            , hslider_ (uciData . engineLines) 1 10 [dragRate 1]
            ]
        ]
    UCIData{..} = _amUciData
    calculatingResponse = isJust _amResponseThread
