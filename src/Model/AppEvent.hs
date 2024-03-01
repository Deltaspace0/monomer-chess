{-# LANGUAGE RecordWildCards #-}

module Model.AppEvent
    ( AppEvent(..)
    , handleEvent
    ) where

import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Control.Lens
import Control.Monad
import Data.IORef
import Data.Maybe
import Data.Text (pack, unpack, Text)
import Data.Text.Encoding
import Data.Tree (Tree(..))
import Game.Chess
import Game.Chess.PGN
import Monomer
import Monomer.Dragboard.DragboardEvent (DragId(..))
import System.Directory
import Text.Megaparsec
import TextShow

import Model.AppModel

data AppEvent
    = AppInit
    | AppSetPosition Position
    | AppSyncBoard
    | AppSyncEvalGroups
    | AppBoardChanged ([[Piece]], Int, Int)
    | AppEditBoardChanged ([[Piece]], Int, Int)
    | AppEditBoardRemove DragId
    | AppSetEditMenu Bool
    | AppSetPromotionMenu Bool
    | AppSetErrorMessage (Maybe Text)
    | AppDoPly (Maybe Ply)
    | AppRunNextPly
    | AppPromote PieceType
    | AppSetResetUciBestMove Bool
    | AppPlayNextResponse
    | AppAbortNextResponse
    | AppResponseCalculated (Maybe Ply, Maybe Text)
    | AppSetResponseThread (Maybe ThreadId)
    | AppPlyNumberChanged Int
    | AppPositionTreePathChanged Int
    | AppUndoMove
    | AppSetGame Game
    | AppLoadPGN
    | AppLoadFEN
    | AppApplyEditChanges
    | AppClearEditBoard
    | AppUpdateFEN
    | AppLoadEngine
    | AppSetEngineLoading Int Bool
    | AppSetRequestMVars Int (Maybe (MVar String, MVar Position))
    | AppSetBestMoveMVars Int (Maybe (MVar String, MVar ()))
    | AppSetEngineLogChan (Maybe (Chan UCILog))
    | AppSetCurrentEngineDepth Int (Maybe Text)
    | AppSetPrincipalVariations Int [(Text, Maybe Ply, Maybe Double)]
    | AppSetOptionsUCI Int UCIOptions
    | AppRunAnalysis
    | AppCompleteEval
    | AppAbortEval
    | AppSetEvalProgress (Maybe Text)
    | AppSetEvalProgressMVars (Maybe (MVar [Int], MVar (Tree PP), MVar ()))
    | AppSetPositionTree (Tree PP)
    | AppSendEngineRequest String
    | AppSetUciLogs Text
    | AppClearUciLogs
    | AppRecordUCILogsChanged Bool
    | AppUciNewSlot
    | AppUciCloneSlot
    | AppSetTablebaseData TablebaseData
    deriving (Eq, Show)

instance NFData Ply where
    rnf x = x `seq` ()

type EventHandle = AppModel -> [AppEventResponse AppModel AppEvent]

handleEvent :: AppEventHandler AppModel AppEvent
handleEvent _ _ model event = case event of
    AppInit -> initHandle model
    AppSetPosition v -> setPositionHandle v model
    AppSyncBoard -> syncBoardHandle model
    AppSyncEvalGroups -> syncEvalGroupsHandle model
    AppBoardChanged info -> boardChangedHandle info model
    AppEditBoardChanged info -> editBoardChangedHandle info model
    AppEditBoardRemove v -> editBoardRemoveHandle v model
    AppSetEditMenu v -> setEditMenuHandle v model
    AppSetPromotionMenu v -> setPromotionMenuHandle v model
    AppSetErrorMessage v -> setErrorMessageHandle v model
    AppDoPly v -> doPlyHandle v model
    AppRunNextPly -> runNextPlyHandle model
    AppPromote pieceType -> promoteHandle pieceType model
    AppSetResetUciBestMove v -> setResetUciBestMoveHandle v model
    AppPlayNextResponse -> playNextResponseHandle model
    AppAbortNextResponse -> abortNextResponseHandle model
    AppResponseCalculated v -> responseCalculatedHandle v model
    AppSetResponseThread v -> setResponseThreadHandle v model
    AppPlyNumberChanged v -> plyNumberChangedHandle v model
    AppPositionTreePathChanged v -> positionTreePathChangedHandle v model
    AppUndoMove -> undoMoveHandle model
    AppSetGame v -> setGameHandle v model
    AppLoadPGN -> loadPGNHandle model
    AppLoadFEN -> loadFENHandle model
    AppApplyEditChanges -> applyEditChangesHandle model
    AppClearEditBoard -> clearEditBoardHandle model
    AppUpdateFEN -> updateFenHandle model
    AppLoadEngine -> loadEngineHandle model
    AppSetEngineLoading i v -> setEngineLoadingHandle i v model
    AppSetRequestMVars i v -> setRequestMVarsHandle i v model
    AppSetBestMoveMVars i v -> setBestMoveMVarsHandle i v model
    AppSetEngineLogChan v -> setEngineLogChanHandle v model
    AppSetCurrentEngineDepth i v -> setCurrentEngineDepthHandle i v model
    AppSetPrincipalVariations i v -> setPrincipalVariationsHandle i v model
    AppSetOptionsUCI i v -> setOptionsUCIHandle i v model
    AppRunAnalysis -> runAnalysisHandle model
    AppCompleteEval -> completeEvalHandle model
    AppAbortEval -> abortEvalHandle model
    AppSetEvalProgress v -> setEvalProgressHandle v model
    AppSetEvalProgressMVars v -> setEvalProgressMVarsHandle v model
    AppSetPositionTree v -> setPositionTreeHandle v model
    AppSendEngineRequest v -> sendEngineRequestHandle v model
    AppSetUciLogs v -> setUciLogsHandle v model
    AppClearUciLogs -> clearUciLogsHandle model
    AppRecordUCILogsChanged v -> recordUCILogsChangedHandle v model
    AppUciNewSlot -> uciNewSlotHandle model
    AppUciCloneSlot -> uciCloneSlotHandle model
    AppSetTablebaseData v -> setTablebaseDataHandle v model

initHandle :: EventHandle
initHandle _ = [Producer producerHandler] where
    producerHandler raiseEvent = do
        createDirectoryIfMissing True "logs_uci"
        logChan <- newChan
        logsListRef <- newIORef []
        logsRef <- newIORef ""
        recordRef <- newIORef False
        raiseEvent $ AppSetEngineLogChan $ Just logChan
        _ <- forkIO $ forever $ do
            threadDelay 500000
            readIORef logsRef >>= raiseEvent . AppSetUciLogs
        let recordLog prefix i logString = do
                let x = prefix <> (show i) <> ": " <> logString
                doRecord <- readIORef recordRef
                when doRecord $ appendFile "logs_uci/outputs.txt" $ x <> "\n"
                modifyIORef logsListRef $ take 32 . (x:)
                readIORef logsListRef >>= writeIORef logsRef . pack . unlines
        forever $ readChan logChan >>= \msg -> case msg of
            LogDoRecord v -> writeIORef recordRef v
            LogClear -> do
                writeIORef logsListRef []
                writeIORef logsRef ""
            LogInput i x -> recordLog "stdin" i x
            LogError i x -> recordLog "stderr" i x
            LogOutput i x -> recordLog "stdout" i x

setPositionHandle :: Position -> EventHandle
setPositionHandle position model@(AppModel{..}) = if isJust _amEvalProgress
    then []
    else
        [ Model $ model
            & chessPosition .~ position
            & positionTree .~ treeFromPosition position
            & positionTreePath .~ []
            & currentPlyNumber .~ 0
            & showPromotionMenu .~ False
            & sanMoves .~ ""
            & forsythEdwards .~ pack (toFEN position)
            & aiData . aiMessage .~ Nothing
        , Event AppSyncBoard
        , Event AppRunAnalysis
        ]

syncBoardHandle :: EventHandle
syncBoardHandle model@(AppModel{..}) = response where
    response =
        [ Model $ model
            & boardState .~ newState
            & boardStateReversed .~ newStateReversed
            & fenData .~ getFenData _amChessPosition
        ]
    (newState, newStateReversed) = getBoardStates _amChessPosition

syncEvalGroupsHandle :: EventHandle
syncEvalGroupsHandle model@(AppModel{..}) = response where
    response = [Model $ model & evalGroups .~ newEvalGroups]
    newEvalGroups = fromJust <$> (filter isJust $ makeEvalGroups evalPoints)
    makeEvalGroups [] = []
    makeEvalGroups (Nothing:xs) = Nothing:(makeEvalGroups xs)
    makeEvalGroups ((Just (x, y)):xs) = result where
        result
            | null gs || null (head gs) = (Just [(x, y), (x, 0)]):gs
            | meet = (Just [(x, y), mp, mp]):(Just $ mp:headGroup):(tail gs)
            | otherwise = (Just $ (x, y):headGroup):(tail gs)
        meet = y*y' <= 0
        mp = (x+evalStep/(1-yDiv), if y' < 0 then -0.0001 else 0.0001)
        yDiv = if y == 0
            then 1000000
            else y'/y
        y' = snd $ head headGroup
        headGroup = fromJust $ head gs
        gs = makeEvalGroups xs
    evalPoints = zipWith makeEvalPoint [0..currentBranchLength] evals
    makeEvalPoint i maybeEval = do
        eval <- maybeEval
        return ((fromIntegral i)*evalStep, (min 6 $ max (-6) eval)*0.027)
    evalStep = 0.96/(fromIntegral currentBranchLength + 0.0001)
    currentBranchLength = length _amPositionTreePath
    evals = _ppEval <$> treeToList _amPositionTreePath _amPositionTree

boardChangedHandle :: ([[Piece]], Int, Int) -> EventHandle
boardChangedHandle info@(_, ixTo, ixFrom) model@(AppModel{..})
    | movingDisabled = [Event AppSyncBoard]
    | _amAutoQueen =
        [ setNextPly
        , Event AppRunNextPly
        , Event AppPlayNextResponse
        ]
    | otherwise =
        [ setNextPly
        , Event $ if noPromotion
            then AppRunNextPly
            else AppSetPromotionMenu True
        , responseIf noPromotion $ Event AppPlayNextResponse
        ]
    where
        movingDisabled = or
            [ isJust _amResponseThread
            , isJust _amEvalProgress
            , abs (ixTo-ixFrom) > 100
            ]
        setNextPly = Model $ model & nextPly .~ Just ply
        ply = getPromotedPly model info Queen
        noPromotion = null $ plyPromotion ply

editBoardChangedHandle :: ([[Piece]], Int, Int) -> EventHandle
editBoardChangedHandle (_, ixTo, ixFrom) model = response where
    response
        | ixTo >= 3000 && ixFrom >= 3000 =
            [ Model $ model & fenData . fenBoardState .~ reversedStateRev
            , Event AppUpdateFEN
            ]
        | ixTo >= 2000 && ixFrom >= 2000 =
            [ Model $ model & fenData . fenBoardStateReversed .~ stateRev
            , Event AppUpdateFEN
            ]
        | ixFrom >= 1000 =
            [ Model modelFromExtraBoard
            , Event AppUpdateFEN
            ]
        | otherwise = []
    stateRev = reverse $ model ^. fenData . fenBoardState
    reversedStateRev = reverse $ model ^. fenData . fenBoardStateReversed
    modelFromExtraBoard = if ixTo >= 3000
        then model
            & fenData . fenBoardState . element (3063-ixTo) .~ piece
            & fenData . fenBoardStateReversed . element (ixTo-3000) .~ piece
        else model
            & fenData . fenBoardState . element (ixTo-2000) .~ piece
            & fenData . fenBoardStateReversed . element (2063-ixTo) .~ piece
    piece = chessPieces!!(ixFrom-1000)

editBoardRemoveHandle :: DragId -> EventHandle
editBoardRemoveHandle (DragId ixFrom) model = response where
    response = if ixFrom < 2000
        then []
        else
            [ Model newModel
            , Event AppUpdateFEN
            ]
    newModel = if ixFrom >= 3000
        then model
            & fenData . fenBoardState . element (3063-ixFrom) .~ []
            & fenData . fenBoardStateReversed . element (ixFrom-3000) .~ []
        else model
            & fenData . fenBoardState . element (ixFrom-2000) .~ []
            & fenData . fenBoardStateReversed . element (2063-ixFrom) .~ []

setEditMenuHandle :: Bool -> EventHandle
setEditMenuHandle v model =
    [ Model $ model
        & showEditMenu .~ v
        & showPromotionMenu .~ False
        & errorMessage .~ Nothing
    ]

setPromotionMenuHandle :: Bool -> EventHandle
setPromotionMenuHandle v model =
    [ Model $ model & showPromotionMenu .~ v
    , responseIf v $ Event AppSyncBoard
    ]

setErrorMessageHandle :: Maybe Text -> EventHandle
setErrorMessageHandle v model =
    [ Model $ model
        & showPromotionMenu .~ False
        & errorMessage .~ v
    ]

doPlyHandle :: Maybe Ply -> EventHandle
doPlyHandle ply model =
    [ Model $ model & nextPly .~ ply
    , Event AppRunNextPly
    ]

runNextPlyHandle :: EventHandle
runNextPlyHandle model@(AppModel{..}) = response where
    response = if isJust _amEvalProgress || null newPosition || not isLegal
        then []
        else
            [ Model $ model
                & chessPosition .~ fromJust newPosition
                & positionTree .~ newPositionTree
                & positionTreePath .~ newTreePath
                & currentPlyNumber +~ 1
                & showPromotionMenu .~ False
                & sanMoves .~ treeToSanMoves newPositionTree
                & forsythEdwards .~ pack (toFEN $ fromJust newPosition)
            , Event AppSyncBoard
            , Event AppRunAnalysis
            ]
    isLegal = (fromJust _amNextPly) `elem` (legalPlies _amChessPosition)
    newPosition = unsafeDoPly _amChessPosition <$> _amNextPly
    newTreePath = case exi of
        Nothing -> cutOffPath <> [0]
        Just i -> if i == _amPositionTreePath!!_amCurrentPlyNumber
            then _amPositionTreePath
            else addTail $ cutOffPath <> [i]
    addTail p = p <> replicate (getTreeTailDepth p _amPositionTree) 0
    (newPositionTree, exi) = insertTree cutOffPath pp _amPositionTree
    cutOffPath = take _amCurrentPlyNumber _amPositionTreePath
    pp = getNextPP ppOld $ fromJust _amNextPly
    ppOld = indexPositionTree model _amCurrentPlyNumber

promoteHandle :: PieceType -> EventHandle
promoteHandle pieceType model = response where
    response =
        [ Model $ model & nextPly %~ ((`promoteTo` pieceType) <$>)
        , Event $ AppSetPromotionMenu False
        , Event AppRunNextPly
        , Event AppPlayNextResponse
        ]

setResetUciBestMoveHandle :: Bool -> EventHandle
setResetUciBestMoveHandle v model = response where
    response = [Model $ model & aiData . resetUciBestMove .~ v]

playNextResponseHandle :: EventHandle
playNextResponseHandle AppModel{..} = response where
    response = [responseIf canRespond $ Producer producerHandler]
    canRespond = null _amEvalProgress || _adResponseMethod /= UCIResponse
    producerHandler raiseEvent = do
        when (_adResponseMethod == UCIResponse) $ do
            let bestSyncVar = snd $ fromJust _uciBestMoveMVars
            when (isJust _uciBestMoveMVars && _adResetUciBestMove) $ do
                takeMVar bestSyncVar
                raiseEvent $ AppSetResetUciBestMove False
        mvar <- newEmptyMVar
        thread <- forkIO $ do
            result <- calculateMove _amChessPosition $ _amAiData
                { _adUciBestMoveMVar = fst <$> _uciBestMoveMVars
                }
            result `deepseq` pure ()
            raiseEvent $ AppResponseCalculated result
            putMVar mvar ()
        raiseEvent $ AppSetResponseThread $ Just thread
        takeMVar mvar
    AIData{..} = _amAiData
    UCIData{..} = _amUciData!!_amUciIndex

abortNextResponseHandle :: EventHandle
abortNextResponseHandle model@(AppModel{..}) =
    [ Model $ model & responseThread .~ Nothing
    , Producer $ const $ maybe (pure ()) killThread _amResponseThread
    ]

responseCalculatedHandle :: (Maybe Ply, Maybe Text) -> EventHandle
responseCalculatedHandle (ply, message) model@(AppModel{..}) = response where
    response = if isJust _amEvalProgress
        then []
        else
            [ Model $ model
                & nextPly .~ ply
                & responseThread .~ Nothing
                & aiData . aiMessage .~ message
                & uciIndex .~ newEngineIndex
            , Event AppRunNextPly
            ]
    newEngineIndex = if _adResponseMethod == UCIResponse && isJust ply
        then _uciEngineNextIndex
        else _amUciIndex
    AIData{..} = _amAiData
    UCIData{..} = _amUciData!!_amUciIndex

setResponseThreadHandle :: Maybe ThreadId -> EventHandle
setResponseThreadHandle v model = [Model $ model & responseThread .~ v]

plyNumberChangedHandle :: Int -> EventHandle
plyNumberChangedHandle newPlyNumber model@(AppModel{..}) = response where
    response = if newPlyNumber < 0 || newPlyNumber > maxPlyNumber
        then []
        else
            [ Model $ model
                & chessPosition .~ _ppPosition
                & currentPlyNumber .~ newPlyNumber
                & showPromotionMenu .~ False
                & forsythEdwards .~ pack (toFEN _ppPosition)
                & aiData . aiMessage .~ Nothing
            , Event AppSyncBoard
            , Event AppRunAnalysis
            ]
    maxPlyNumber = length _amPositionTreePath
    PP{..} = indexPositionTree model newPlyNumber

positionTreePathChangedHandle :: Int -> EventHandle
positionTreePathChangedHandle v model@(AppModel{..}) = response where
    response =
        [ Model $ model
            & chessPosition .~ _ppPosition
            & positionTreePath .~ newTreePath
            & showPromotionMenu .~ False
            & forsythEdwards .~ pack (toFEN _ppPosition)
            & aiData . aiMessage .~ Nothing
        , Event AppSyncBoard
        , Event AppRunAnalysis
        ]
    Node PP{..} _ = indexTree path _amPositionTree
    path = take _amCurrentPlyNumber newTreePath
    newTreePath = initTreePath <> replicate tailDepth 0
    tailDepth = getTreeTailDepth initTreePath _amPositionTree
    initTreePath = take (_amCurrentPlyNumber-1) _amPositionTreePath <> [v]

undoMoveHandle :: EventHandle
undoMoveHandle model@(AppModel{..}) = response where
    response = if isJust _amEvalProgress
        then []
        else
            [ Model $ model
                & chessPosition .~ _ppPosition
                & positionTree .~ newPositionTree
                & positionTreePath .~ newTreePath
                & currentPlyNumber .~ length newTreePath
                & showPromotionMenu .~ False
                & sanMoves .~ treeToSanMoves newPositionTree
                & forsythEdwards .~ pack (toFEN _ppPosition)
                & aiData . aiMessage .~ Nothing
            , Event AppSyncBoard
            , Event AppRunAnalysis
            ]
    Node PP{..} _ = indexTree newTreePath newPositionTree
    newTreePath = initTreePath <> replicate tailDepth 0
    tailDepth = getTreeTailDepth initTreePath newPositionTree
    initTreePath = init _amPositionTreePath
    newPositionTree = pruneTree _amPositionTreePath _amPositionTree

setGameHandle :: Game -> EventHandle
setGameHandle CG{..} model@(AppModel{..}) = response where
    response = if isJust _amEvalProgress
        then []
        else
            [ Model $ model
                & chessPosition .~ position
                & positionTree .~ tree
                & positionTreePath .~ newTreePath
                & currentPlyNumber .~ 0
                & showPromotionMenu .~ False
                & forsythEdwards .~ fromMaybe (pack (toFEN startpos)) gameFEN
                & aiData . aiMessage .~ Nothing
            , Event AppSyncBoard
            , Event AppRunAnalysis
            ]
    position = fromMaybe startpos $ gameFEN >>= fromFEN . unpack
    gameFEN = lookup "FEN" _cgTags
    newTreePath = replicate (getTreeTailDepth [] tree) 0
    tree = toPositionTree position $ (_annPly <$>) <$> _cgForest

loadPGNHandle :: EventHandle
loadPGNHandle AppModel{..} = response where
    response = case parseMaybe pgn (encodeUtf8 $ _amSanMoves <> "*") of
        Just (PGN []) -> []
        Just (PGN (x:_)) -> [Event $ AppSetGame x]
        Nothing -> [Event $ AppSetErrorMessage $ Just "Invalid PGN"]

loadFENHandle :: EventHandle
loadFENHandle AppModel{..} = [Task taskHandler] where
    taskHandler = do
        let newPosition = fromFEN $ unpack _amForsythEdwards
            x = fromJust newPosition
        catch (x `seq` return (AppSetPosition x)) f
    f :: ErrorCall -> IO AppEvent
    f = const $ return $ AppSetErrorMessage $ Just "Invalid FEN"

applyEditChangesHandle :: EventHandle
applyEditChangesHandle model =
    [ Model $ model & showEditMenu .~ False
    , Event AppLoadFEN
    ]

clearEditBoardHandle :: EventHandle
clearEditBoardHandle model =
    [ Model $ model
        & fenData . fenBoardState .~ replicate 64 []
        & fenData . fenBoardStateReversed .~ replicate 64 []
        & fenData . fenCastleWK .~ False
        & fenData . fenCastleWQ .~ False
        & fenData . fenCastleBK .~ False
        & fenData . fenCastleBQ .~ False
    , Event AppUpdateFEN
    ]

updateFenHandle :: EventHandle
updateFenHandle model@(AppModel{..}) = response where
    response = [Model $ model & forsythEdwards .~ newFEN]
    newFEN = pack $ toFEN $ fromJust $ fromFEN $ getFenString _amFenData

loadEngineHandle :: EventHandle
loadEngineHandle AppModel{..} = [Producer producerHandler] where
    producerHandler raiseEvent = loadUciEngine currentUciData $ f raiseEvent
    f raiseEvent event = raiseEvent $ case event of
        EventReportError v -> AppSetErrorMessage $ Just v
        EventSetEngineLoading v -> AppSetEngineLoading _amUciIndex v
        EventSetRequestMVars v -> AppSetRequestMVars _amUciIndex v
        EventSetBestMoveMVars v -> AppSetBestMoveMVars _amUciIndex v
        EventSetCurrentDepth v -> AppSetCurrentEngineDepth _amUciIndex v
        EventSetPV v -> AppSetPrincipalVariations _amUciIndex v
        EventSetOptionsUCI v -> AppSetOptionsUCI _amUciIndex v
    currentUciData = _amUciData!!_amUciIndex

setEngineLoadingHandle :: Int -> Bool -> EventHandle
setEngineLoadingHandle i v model = response where
    response = [Model $ model & uciData . ix i . engineLoading .~ v]

setRequestMVarsHandle
    :: Int
    -> Maybe (MVar String, MVar Position)
    -> EventHandle
setRequestMVarsHandle i v model = response where
    response = [Model $ model & uciData . ix i . requestMVars .~ v]

setBestMoveMVarsHandle :: Int -> Maybe (MVar String, MVar ()) -> EventHandle
setBestMoveMVarsHandle i v model = response where
    response = [Model $ model & uciData . ix i . bestMoveMVars .~ v]

setEngineLogChanHandle :: Maybe (Chan UCILog) -> EventHandle
setEngineLogChanHandle v model = response where
    response = [Model $ model & uciData %~ map (engineLogChan .~ v)]

setCurrentEngineDepthHandle :: Int -> Maybe Text -> EventHandle
setCurrentEngineDepthHandle i v model = response where
    response = [Model $ model & uciData . ix i . currentEngineDepth .~ v]

setPrincipalVariationsHandle
    :: Int
    -> [(Text, Maybe Ply, Maybe Double)]
    -> EventHandle
setPrincipalVariationsHandle i v model@(AppModel{..}) = response where
    response =
        [ Model $ model & uciData . ix i . principalVariations .~ v
        , Producer producerHandler
        ]
    producerHandler raiseEvent = do
        let (assignmentMVar, treeMVar, _) = fromJust _amEvalProgressMVars
            bestSyncVar = snd $ fromJust _uciBestMoveMVars
        when (isJust _amEvalProgressMVars && isJust newEval) $ do
            assignments <- readMVar assignmentMVar
            let j = assignments!!i
            when (i < length assignments && j >= 0) $ do
                tree <- takeMVar treeMVar
                let Node dummyValue _ = tree
                    offsetTree = Node dummyValue [tree]
                    cutOffPath = take j $ 0:_amPositionTreePath
                    ppOld = indexPositionTree model j
                    pp = ppOld {_ppEval = newEval}
                    Node _ nodes = fst $ insertTree cutOffPath pp offsetTree
                    newPositionTree = head nodes
                raiseEvent $ AppSetPositionTree newPositionTree
                putMVar treeMVar newPositionTree
                when (isJust _uciBestMoveMVars) $ do
                    _ <- tryTakeMVar bestSyncVar
                    putMVar bestSyncVar ()
    newEval = listToMaybe v >>= \(_, _, x) -> x
    UCIData{..} = _amUciData!!i

setOptionsUCIHandle :: Int -> UCIOptions -> EventHandle
setOptionsUCIHandle i v model = response where
    response = [Model $ model & uciData . ix i . optionsUCI .~ v]

runAnalysisHandle :: EventHandle
runAnalysisHandle model@(AppModel{..}) = response where
    response =
        [ Model $ model & aiData . resetUciBestMove .~ True
        , if _amShowTablebase
            then Producer tablebaseHandler
            else Model $ model & tablebaseData .~ defaultTablebaseData
        , responseIf (null _amEvalProgress) $ Producer uciHandler
        ]
    tablebaseHandler raiseEvent = do
        raiseEvent $ AppSetTablebaseData $ _amTablebaseData
            { _tbdStatusMessage = Just "Querying..."
            }
        newTablebaseData <- tablebaseRequestAnalysis pos
        raiseEvent $ AppSetTablebaseData newTablebaseData
    uciHandler _ = do
        let (bestMoveVar, bestSyncVar) = fromJust _uciBestMoveMVars
        when (isJust _uciBestMoveMVars) $ do
            _ <- tryTakeMVar bestMoveVar
            _ <- tryTakeMVar bestSyncVar
            putMVar bestSyncVar ()
        uciRequestAnalysis currentUciData initPos pos uciMoves
    pos = _ppPosition currentPP
    initPos = _ppPosition $ indexPositionTree model 0
    uciMoves = _ppUciMoves currentPP
    currentPP = indexPositionTree model _amCurrentPlyNumber
    currentUciData@(UCIData{..}) = _amUciData!!_amUciIndex

completeEvalHandle :: EventHandle
completeEvalHandle model@(AppModel{..}) = response where
    response =
        [ Model $ model & evalProgress .~ Just "Waiting for UCI engines..."
        , Producer producerHandler
        ]
    producerHandler raiseEvent = do
        progressCounter <- newMVar 0
        taskQueue <- newMVar [0..currentBranchLength]
        assignmentMVar <- newMVar $ _amUciData >> [-1]
        treeMVar <- newMVar _amPositionTree
        allDone <- newEmptyMVar
        indefBlocker <- newEmptyMVar
        let progressMVars = (assignmentMVar, treeMVar, allDone)
        raiseEvent $ AppSetEvalProgressMVars $ Just progressMVars
        threads <- forM _amUciData $ \x@(UCIData{..}) -> forkIO $ forever $ do
            let (bestMoveVar, bestSyncVar) = fromJust _uciBestMoveMVars
            when (null _uciBestMoveMVars) $ takeMVar indefBlocker
            queue <- takeMVar taskQueue
            when (not $ null queue) $ do
                let pn = head queue
                    PP{..} = indexPositionTree model pn
                putMVar taskQueue $ tail queue
                _ <- tryTakeMVar bestMoveVar
                uciRequestAnalysis x initPos _ppPosition _ppUciMoves
                _ <- takeMVar bestMoveVar
                assignments1 <- takeMVar assignmentMVar
                let newAssignments1 = assignments1 & ix _uciEngineIndex .~ pn
                putMVar assignmentMVar newAssignments1
                takeMVar bestSyncVar
                assignments2 <- takeMVar assignmentMVar
                let newAssignments2 = assignments2 & ix _uciEngineIndex .~ -1
                putMVar assignmentMVar newAssignments2
                currentProgress <- takeMVar progressCounter
                let newProgress = currentProgress+1
                if currentProgress >= currentBranchLength
                    then putMVar allDone ()
                    else putMVar progressCounter newProgress
                raiseEvent $ AppSetEvalProgress $ showProgress newProgress
        takeMVar allDone
        forM_ threads killThread
        raiseEvent $ AppSetEvalProgress Nothing
        threadDelay 200000
        raiseEvent AppSyncEvalGroups
    showProgress x = Just $ "Done " <> (showt x) <> progressTextSuffix
    progressTextSuffix = "/" <> (showt $ currentBranchLength+1)
    currentBranchLength = length _amPositionTreePath
    initPos = _ppPosition $ indexPositionTree model 0

abortEvalHandle :: EventHandle
abortEvalHandle model@(AppModel{..}) = response where
    response = if null _amEvalProgressMVars
        then []
        else
            [ Model $ model
                & evalProgress .~ Nothing
                & evalProgressMVars .~ Nothing
            , Producer producerHandler
            ]
    producerHandler _ = putMVar doneMVar ()
    (_, _, doneMVar) = fromJust _amEvalProgressMVars

setEvalProgressHandle :: Maybe Text -> EventHandle
setEvalProgressHandle v model = [Model $ model & evalProgress .~ v]

setEvalProgressMVarsHandle
    :: Maybe (MVar [Int], MVar (Tree PP), MVar ())
    -> EventHandle
setEvalProgressMVarsHandle v model = response where
    response = [Model $ model & evalProgressMVars .~ v]

setPositionTreeHandle :: Tree PP -> EventHandle
setPositionTreeHandle v model = [Model $ model & positionTree .~ v]

sendEngineRequestHandle :: String -> EventHandle
sendEngineRequestHandle v AppModel{..} = [Producer producerHandler] where
    producerHandler _ = maybe (pure ()) sendRequest _uciRequestMVars
    sendRequest = flip putMVar v . fst
    UCIData{..} = _amUciData!!_amUciIndex

setUciLogsHandle :: Text -> EventHandle
setUciLogsHandle v model = [Model $ model & uciLogs .~ v]

clearUciLogsHandle :: EventHandle
clearUciLogsHandle model@(AppModel{..}) = response where
    response =
        [ Producer producerHandler
        , Model $ model & uciLogs .~ ""
        ]
    producerHandler _ = maybe (pure ()) f _uciEngineLogChan
    f = flip writeChan LogClear
    UCIData{..} = _amUciData!!_amUciIndex

recordUCILogsChangedHandle :: Bool -> EventHandle
recordUCILogsChangedHandle v AppModel{..} = [Producer producerHandler] where
    producerHandler _ = maybe (pure ()) f _uciEngineLogChan
    f = flip writeChan $ LogDoRecord v
    UCIData{..} = _amUciData!!_amUciIndex

uciNewSlotHandle :: EventHandle
uciNewSlotHandle model@(AppModel{..}) = response where
    response =
        [ Model $ model
            & uciData <>~ [newUciData]
            & uciIndex .~ newEngineIndex
        ]
    newUciData = defaultUciData
        & engineIndex .~ newEngineIndex
        & engineNextIndex .~ newEngineIndex
        & engineLogChan .~ _uciEngineLogChan (head _amUciData)
    newEngineIndex = length _amUciData

uciCloneSlotHandle :: EventHandle
uciCloneSlotHandle model@(AppModel{..}) = response where
    response =
        [ Model $ model
            & uciData <>~ [newUciData]
            & uciIndex .~ newEngineIndex
        ]
    newUciData = defaultUciData
        & engineIndex .~ newEngineIndex
        & engineNextIndex .~ newEngineIndex
        & enginePath .~ _uciEnginePath
        & engineDepth .~ _uciEngineDepth
        & engineNodes .~ _uciEngineNodes
        & engineDepthOrNodes .~ _uciEngineDepthOrNodes
        & engineLogChan .~ _uciEngineLogChan
    newEngineIndex = length _amUciData
    UCIData{..} = _amUciData!!_amUciIndex

setTablebaseDataHandle :: TablebaseData -> EventHandle
setTablebaseDataHandle v model = [Model $ model & tablebaseData .~ v]
