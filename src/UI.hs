{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module UI
    ( buildUI
    ) where

import Data.Maybe
import Data.Tree (Tree(..))
import Game.Chess
import Monomer hiding (Color)
import Monomer.Checkerboard
import Monomer.Dragboard
import TextShow

import Composites
import Model

buildUI :: UIBuilder AppModel AppEvent
buildUI _ model@(AppModel{..}) = tree where
    tree = keystroke keyShortcuts $ hstack'
        [ zstack
            [ vstack' $ dropRemoveCont <$>
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
                , vscroll $ vstack' $ labelPV <$> _uciPrincipalVariations
                , filler
                ]
            , widgetIf _amShowPromotionMenu promAlert
            ] `styleBasic` [sizeReqW $ fixedSize 400]
        , separatorLine
        , dropRemoveCont $ vstack'
            [ if _amShowTablebase
                then tablebasePanel tablebaseData AppDoPly
                else moveHistoryPanel
            , filler
            , separatorLine
            , toggleButton "Tablebase" showTablebase
            ] `styleBasic` [sizeReqW $ fixedSize 204]
        , separatorLine
        , zstack
            [ rightPanel
            , widgetIf (_amShowPromotionMenu && _amShowTwoBoards) promAlert
            , widgetIf (not $ null _amErrorMessage) $
                alertMsg (fromMaybe "" _amErrorMessage) $
                    AppSetErrorMessage Nothing
            ]
        ] `styleBasic` [padding 16]
    labelPV (caption, ply) = box_ [onClick $ AppDoPly ply] $ label caption
    dropRemoveCont = dropTarget AppEditBoardRemove
    keyShortcuts = zipWith (,) ["Up", "Left", "Right", "Down"] plyEvents
    moveHistoryPanel = vstack'
        [ moveHistoryButtons
        , separatorLine
        , vscroll $ vstack_ [childSpacing_ 4] moveLines
        ]
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
        , length _amPositionTreePath
        ]
    notFirstPosition = _amCurrentPlyNumber > 0
    notLastPosition = _amCurrentPlyNumber < length _amPositionTreePath
    moveLines = makeHistoryLine <$> if firstMoveColor == White
        then [0..(length _amPositionTreePath + 1) `div` 2 - 1]
        else [0..(length _amPositionTreePath + 2) `div` 2 - 1]
    makeHistoryLine i = hstack
        [ labelS (i+1) `styleBasic` [sizeReqW $ fixedSize 30]
        , hgrid_ [childSpacing_ 4]
            [ if i1 < 1
                then filler
                else plyHistoryButton i1 `nodeKey` ("his" <> (showt i1))
            , if length _amPositionTreePath-i2 < 0
                then filler
                else plyHistoryButton i2 `nodeKey` ("his" <> (showt i2))
            ] `styleBasic` [sizeReqW $ fixedSize 164]
        ] where
            (i1, i2) = if firstMoveColor == White
                then (i*2+1, i*2+2)
                else (i*2, i*2+1)
    plyHistoryButton i = result where
        result = if i /= _amCurrentPlyNumber || noChoice
            then optionButton_ sanCaption i currentPlyNumber
                [onChange AppPlyNumberChanged]
            else textDropdownV_ currentPathIndex AppPositionTreePathChanged
                [0..length childNodes-1] convertPathToSan []
        sanCaption = if noChoice
            then t
            else t <> "..."
        noChoice = length childNodes < 2
        Node (_, _, _, t) _ = childNodes!!currentPathIndex
        Node _ childNodes = indexTree slicePath _amPositionTree
        currentPathIndex = _amPositionTreePath!!(i-1)
        convertPathToSan pathIndex = resultSan where
            Node (_, _, _, resultSan) _ = childNodes!!pathIndex
        slicePath = take (i-1) _amPositionTreePath
    firstMoveColor = let (p, _, _, _) = indexPositionTree model 0 in color p
    rightPanel = vstack' $ dropRemoveCont <$> if _amShowTwoBoards
        then
            [ box' $ chessBoardRight `styleBasic`
                [ sizeReqW $ fixedSize 400
                , sizeReqH $ fixedSize 400
                ]
            , separatorLine
            , buttonPanel
            , filler
            ]
        else
            [ widgetIf (not _amShowEditMenu) $ vstack'
                [ zstack
                    [ label "Moves (PGN)"
                    , box_ [alignRight] $ button "Import PGN" AppLoadPGN
                    ]
                , textArea sanMoves `styleBasic` [sizeReqH $ fixedSize 128]
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
            , filler
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
        , vscroll $ vstack'
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
                    , button "Undo move" AppUndoMove `nodeEnabled` canUndo
                    ]
            , button "Refresh analysis" AppRunAnalysis
            ]
    canUndo = let (Node _ xs) = _amPositionTree in not $ null xs
    resetTwoBoardsButtons = hgrid'
        [ button "Reset board" (AppSetPosition startpos)
            `nodeEnabled` not calculatingResponse
        , toggleButton "Two boards" showTwoBoards
        ]
    promAlert = alert (AppSetPromotionMenu False) $ vstack'
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
        , moveValidator $ validateMove model
        , onChange AppBoardChanged
        ]
    gameBoardR = dragboard_ 8 8 boardStateReversed checkerPath
        [ checkerConfig [lightColor gray]
        , moveValidator $ validateMove model
        , dragIdOffset 500
        , onChange AppBoardChanged
        ]
    editBoard = dragboard_ 8 8 (fenData . fenBoardState) checkerPath
        [ checkerConfig [lightColor gray, darkColor darkGray]
        , dragIdOffset 2000
        , onChange AppEditBoardChanged
        ]
    editBoardR = dragboard_ 8 8 (fenData . fenBoardStateReversed) checkerPath
        [ checkerConfig [lightColor gray, darkColor darkGray]
        , dragIdOffset 3000
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
        else if isWhiteTurn model
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
            [ labeledRadio_ ("Engine depth: " <> (showt _uciEngineDepth))
                True (uciData . engineDepthOrNodes) [textRight]
            , hslider_ (uciData . engineDepth) 1 100 [dragRate 1]
            ]
        , hgrid'
            [ labeledRadio_ "Engine nodes:" False
                (uciData . engineDepthOrNodes) [textRight]
            , numericField $ uciData . engineNodes
            ]
        , separatorLine
        , label "UCI options"
        , if null _uciRequestMVars
            then label "Not available (UCI is not loaded)"
            else uciOptionsPanel (uciData . optionsUCI) $
                fst $ fromJust _uciRequestMVars
        ]
    UCIData{..} = _amUciData
    calculatingResponse = isJust _amResponseThread
