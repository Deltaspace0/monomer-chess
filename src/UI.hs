{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module UI
    ( buildUI
    ) where

import Control.Lens
import Data.Maybe
import Data.Tree (Tree(..))
import Game.Chess
import Monomer hiding (Color)
import Monomer.Checkerboard
import Monomer.Dragboard
import Monomer.Graph
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
    labelPV (caption, ply, _) = box_ [onClick $ AppDoPly ply] $ label caption
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
        , currentBranchLength
        ]
    currentBranchLength = length _amPositionTreePath
    notFirstPosition = _amCurrentPlyNumber > 0
    notLastPosition = _amCurrentPlyNumber < currentBranchLength
    moveLines = makeHistoryLine <$> if firstMoveColor == White
        then [0..(currentBranchLength + 1) `div` 2 - 1]
        else [0..(currentBranchLength + 2) `div` 2 - 1]
    makeHistoryLine i = hstack
        [ labelS (i+1) `styleBasic` [sizeReqW $ fixedSize 30]
        , hgrid_ [childSpacing_ 4]
            [ if i1 < 1
                then filler
                else plyHistoryButton i1 `nodeKey` ("his" <> (showt i1))
            , if currentBranchLength-i2 < 0
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
            then _ppSan pp
            else _ppSan pp <> "..."
        noChoice = length childNodes < 2
        Node pp _ = childNodes!!currentPathIndex
        Node _ childNodes = indexTree slicePath _amPositionTree
        currentPathIndex = _amPositionTreePath!!(i-1)
        convertPathToSan pathIndex = _ppSan where
            Node PP{..} _ = childNodes!!pathIndex
        slicePath = take (i-1) _amPositionTreePath
    firstMoveColor = color $ _ppPosition $ indexPositionTree model 0
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
            [ hstack'
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
            [ zstack
                [ label "Moves (PGN)"
                , box_ [alignRight] $ button "Import PGN" AppLoadPGN
                ]
            , textArea sanMoves `styleBasic` [sizeReqH $ fixedSize 128]
            , separatorLine
            , labeledCheckbox' "Rotate board" boardRotated
            , labeledCheckbox' "Auto promote to queen" autoQueen
            , separatorLine
            , aiPanel aiData
            , widgetIf (_adResponseMethod == UCIResponse) $ hstack'
                [ label "After response switch to"
                , textDropdown_ (uciData' . engineNextIndex)
                    [0..length _amUciData-1] (("UCI" <>) . showt) []
                ]
            , separatorLine
            , zstack
                [ label "Analysis graph"
                , box_ [alignRight] $ hstack'
                    [ if null _amEvalProgress
                        then button "Complete eval" AppCompleteEval
                        else button "Abort" AppAbortEval
                    , button "Refresh" AppSyncEvalGroups
                    ]
                ]
            , widgetIf (isJust _amEvalProgress) $
                label $ fromMaybe "" _amEvalProgress
            , graphWithData_ (graphDataEval <> [currentMoveGraphData])
                [ lockX
                , lockY
                , hideGrid
                , limitX (-0.02, 0.98)
                , limitY (-0.171, 0.171)
                , onRightClick AppGraphClicked
                ] `styleBasic`
                    [ sizeReqH $ fixedSize 128
                    , bgColor $ rgb 42 34 69
                    ]
            , label "Right click on graph to set the position on that move"
            , separatorLine
            , uciPanel
            ] `styleBasic` [padding 8]
        ]
    graphDataEval = _amEvalGroups >>= \evalGroup ->
        [
            [ graphPoints evalGroup
            , graphColor $ if snd (head evalGroup) < 0
                then black
                else white
            , graphFill
            , graphWidth 0
            , graphDuration 200
            ]
        ,   [ graphPoints $ init evalGroup
            , graphColor orange
            , graphWidth 1
            , graphDuration 200
            ]
        ]
    currentMoveGraphData =
        [ graphPoints
            [ (evalStep*(fromIntegral _amCurrentPlyNumber), -0.17)
            , (evalStep*(fromIntegral _amCurrentPlyNumber), 0.17)
            ]
        , graphColor yellow
        , graphWidth 1
        , graphDuration 200
        ]
    evalStep = 0.96/(fromIntegral currentBranchLength + 0.0001)
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
    calculatingResponse = isJust _amResponseThread
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
        [ zstack
            [ label "UCI settings"
            , box_ [alignRight] $ hstack'
                [ textDropdown_ uciIndex [0..length _amUciData-1]
                    (("UCI" <>) . showt) [onChange uciIndexEvent] `styleBasic`
                        [sizeReqW $ fixedSize 100]
                , button "Clone" AppUciCloneSlot
                , button "New" AppUciNewSlot
                ]
            ]
        , hstack_ [childSpacing_ 16]
            [ label "Path: "
            , textField $ uciData' . enginePath
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
        , labeledCheckbox' "Live PV report" $ uciData' . engineLiveReport
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
                True (uciData' . engineDepthOrNodes) [textRight]
            , hslider_ (uciData' . engineDepth) 1 100 [dragRate 1]
            ]
        , hgrid'
            [ labeledRadio_ "Engine nodes:" False
                (uciData' . engineDepthOrNodes) [textRight]
            , numericField $ uciData' . engineNodes
            ]
        , separatorLine
        , label "UCI options"
        , if null _uciRequestMVars
            then label "Not available (UCI is not loaded)"
            else uciOptionsPanel (uciData' . optionsUCI) $
                fst $ fromJust _uciRequestMVars
        ]
    uciIndexEvent :: Int -> AppEvent
    uciIndexEvent _ = AppRunAnalysis
    uciData' :: Lens' AppModel UCIData
    uciData' = uciData . ixl _amUciIndex
    ixl :: Int -> Lens' [UCIData] UCIData
    ixl i = lens (!!i) (\x v -> x & ix i .~ v)
    AIData{..} = _amAiData
    UCIData{..} = _amUciData!!_amUciIndex
