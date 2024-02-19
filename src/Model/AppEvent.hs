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

import Model.AppModel

data AppEvent
    = AppInit
    | AppSetPosition Position
    | AppSyncBoard
    | AppBoardChanged ([[Piece]], Int, Int)
    | AppEditBoardChanged ([[Piece]], Int, Int)
    | AppEditBoardRemove DragId
    | AppSetEditMenu Bool
    | AppSetPromotionMenu Bool
    | AppSetErrorMessage (Maybe Text)
    | AppDoPly (Maybe Ply)
    | AppRunNextPly
    | AppPromote PieceType
    | AppSetUciBestMoveMVar (Maybe (MVar String, MVar ()))
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
    | AppSetEngineLoading Bool
    | AppSetRequestMVars (Maybe (MVar String, MVar Position))
    | AppSetEngineLogChan (Maybe (Chan String))
    | AppSetCurrentEngineDepth (Maybe Text)
    | AppSetPrincipalVariations [(Text, Maybe Ply)]
    | AppSetOptionsUCI UCIOptions
    | AppRunAnalysis
    | AppSendEngineRequest String
    | AppSetUciLogs Text
    | AppClearUciLogs
    | AppRecordUCILogsChanged Bool
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
    AppBoardChanged info -> boardChangedHandle info model
    AppEditBoardChanged info -> editBoardChangedHandle info model
    AppEditBoardRemove v -> editBoardRemoveHandle v model
    AppSetEditMenu v -> setEditMenuHandle v model
    AppSetPromotionMenu v -> setPromotionMenuHandle v model
    AppSetErrorMessage v -> setErrorMessageHandle v model
    AppDoPly v -> doPlyHandle v model
    AppRunNextPly -> runNextPlyHandle model
    AppPromote pieceType -> promoteHandle pieceType model
    AppSetUciBestMoveMVar v -> setUciBestMoveMVarHandle v model
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
    AppSetEngineLoading v -> setEngineLoadingHandle v model
    AppSetRequestMVars v -> setRequestMVarsHandle v model
    AppSetEngineLogChan v -> setEngineLogChanHandle v model
    AppSetCurrentEngineDepth v -> setCurrentEngineDepthHandle v model
    AppSetPrincipalVariations v -> setPrincipalVariationsHandle v model
    AppSetOptionsUCI v -> setOptionsUCIHandle v model
    AppRunAnalysis -> runAnalysisHandle model
    AppSendEngineRequest v -> sendEngineRequestHandle v model
    AppSetUciLogs v -> setUciLogsHandle v model
    AppClearUciLogs -> clearUciLogsHandle model
    AppRecordUCILogsChanged v -> recordUCILogsChangedHandle v model
    AppSetTablebaseData v -> setTablebaseDataHandle v model

initHandle :: EventHandle
initHandle _ = [Producer producerHandler] where
    producerHandler raiseEvent = do
        createDirectoryIfMissing True "logs_uci"
        logChan <- newChan
        bestMoveVar <- newEmptyMVar
        bestSyncVar <- newEmptyMVar
        raiseEvent $ AppSetEngineLogChan $ Just logChan
        raiseEvent $ AppSetUciBestMoveMVar $ Just (bestMoveVar, bestSyncVar)
        logsListRef <- newIORef []
        logsRef <- newIORef ""
        recordRef <- newIORef False
        _ <- forkIO $ forever $ do
            threadDelay 500000
            readIORef logsRef >>= raiseEvent . AppSetUciLogs
        forever $ readChan logChan >>= \x -> case x of
            "monomer-record-uci-True" -> writeIORef recordRef True
            "monomer-record-uci-False" -> writeIORef recordRef False
            "monomer-clear-uci-logs" -> do
                writeIORef logsListRef []
                writeIORef logsRef ""
            _ -> do
                let uciBestMove = getUciBestMove x
                when (isJust uciBestMove) $ do
                    _ <- tryTakeMVar bestMoveVar
                    putMVar bestMoveVar $ fromJust uciBestMove
                doRecord <- readIORef recordRef
                when doRecord $ appendFile "logs_uci/outputs.txt" $ x <> "\n"
                modifyIORef logsListRef $ take 32 . (x:)
                readIORef logsListRef >>= writeIORef logsRef . pack . unlines

setPositionHandle :: Position -> EventHandle
setPositionHandle position model =
    [ Model $ model
        & chessPosition .~ position
        & positionTree .~ Node (position, Nothing, "", "") []
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

boardChangedHandle :: ([[Piece]], Int, Int) -> EventHandle
boardChangedHandle info@(_, ixTo, ixFrom) model@(AppModel{..})
    | isJust _amResponseThread || differentBoards = [Event AppSyncBoard]
    | _amAutoQueen =
        [ setNextPly
        , Event AppRunNextPly
        , responseIf _amAutoRespond $ Event AppPlayNextResponse
        ]
    | otherwise =
        [ setNextPly
        , Event $ if noPromotion
            then AppRunNextPly
            else AppSetPromotionMenu True
        , responseIf (_amAutoRespond && noPromotion) $
            Event AppPlayNextResponse
        ]
    where
        differentBoards = abs (ixTo-ixFrom) > 100
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
    response = if null newPosition || not isLegal
        then []
        else
            [ Model $ model
                & chessPosition .~ fromJust newPosition
                & positionTree .~ newPositionTree
                & positionTreePath .~ newPositionTreePath
                & currentPlyNumber +~ 1
                & showPromotionMenu .~ False
                & sanMoves .~ treeToSanMoves newPositionTree
                & forsythEdwards .~ pack (toFEN $ fromJust newPosition)
            , Event AppSyncBoard
            , Event AppRunAnalysis
            ]
    isLegal = (fromJust _amNextPly) `elem` (legalPlies _amChessPosition)
    newPosition = unsafeDoPly _amChessPosition <$> _amNextPly
    newPositionTree = case insertResult of
        Left _ -> _amPositionTree
        Right tree -> tree
    newPositionTreePath = case insertResult of
        Left exi -> if exi == _amPositionTreePath!!_amCurrentPlyNumber
            then _amPositionTreePath
            else addTail $ cutOffPositionTreePath <> [exi]
        Right _ -> cutOffPositionTreePath <> [0]
    addTail p = p <> replicate (getTreeTailDepth p _amPositionTree) 0
    insertResult = insertTree cutOffPositionTreePath pp _amPositionTree
    cutOffPositionTreePath = take _amCurrentPlyNumber _amPositionTreePath
    pp = getNextPP ppOld $ fromJust _amNextPly
    ppOld = indexPositionTree model _amCurrentPlyNumber

promoteHandle :: PieceType -> EventHandle
promoteHandle pieceType model@(AppModel{..}) = response where
    response =
        [ Model $ model & nextPly %~ ((`promoteTo` pieceType) <$>)
        , Event $ AppSetPromotionMenu False
        , Event AppRunNextPly
        , responseIf _amAutoRespond $ Event AppPlayNextResponse
        ]

setUciBestMoveMVarHandle :: Maybe (MVar String, MVar ()) -> EventHandle
setUciBestMoveMVarHandle v model = response where
    response = [Model $ model & aiData . uciBestMoveMVar .~ v]

setResetUciBestMoveHandle :: Bool -> EventHandle
setResetUciBestMoveHandle v model = response where
    response = [Model $ model & aiData . resetUciBestMove .~ v]

playNextResponseHandle :: EventHandle
playNextResponseHandle AppModel{..} = response where
    response = [Producer producerHandler]
    producerHandler raiseEvent = do
        let (bestMoveVar, bestSyncVar) = fromJust _adUciBestMoveMVar
        when (isJust _adUciBestMoveMVar) $ do
            when _adResetUciBestMove $ do
                takeMVar bestSyncVar
                raiseEvent $ AppSetResetUciBestMove False
            when (null _uciRequestMVars) $ do
                _ <- tryTakeMVar bestMoveVar
                putMVar bestMoveVar "nouci"
        mvar <- newEmptyMVar
        thread <- forkIO $ do
            result <- calculateMove _amChessPosition _amAiData
            result `deepseq` pure ()
            raiseEvent $ AppResponseCalculated result
            putMVar mvar ()
        raiseEvent $ AppSetResponseThread $ Just thread
        takeMVar mvar
    AIData{..} = _amAiData
    UCIData{..} = _amUciData

abortNextResponseHandle :: EventHandle
abortNextResponseHandle model@(AppModel{..}) =
    [ Model $ model & responseThread .~ Nothing
    , Producer $ const $ maybe (pure ()) killThread _amResponseThread
    ]

responseCalculatedHandle :: (Maybe Ply, Maybe Text) -> EventHandle
responseCalculatedHandle (responsePly, message) model =
    [ Model $ model
        & nextPly .~ responsePly
        & responseThread .~ Nothing
        & aiData . aiMessage .~ message
    , Event AppRunNextPly
    ]

setResponseThreadHandle :: Maybe ThreadId -> EventHandle
setResponseThreadHandle v model = [Model $ model & responseThread .~ v]

plyNumberChangedHandle :: Int -> EventHandle
plyNumberChangedHandle newPlyNumber model@(AppModel{..}) = response where
    response = if newPlyNumber < 0 || newPlyNumber > maxPlyNumber
        then []
        else
            [ Model $ model
                & chessPosition .~ position
                & currentPlyNumber .~ newPlyNumber
                & showPromotionMenu .~ False
                & forsythEdwards .~ pack (toFEN position)
                & aiData . aiMessage .~ Nothing
            , Event AppSyncBoard
            , Event AppRunAnalysis
            ]
    maxPlyNumber = length _amPositionTreePath
    (position, _, _, _) = indexPositionTree model newPlyNumber

positionTreePathChangedHandle :: Int -> EventHandle
positionTreePathChangedHandle v model@(AppModel{..}) = response where
    response =
        [ Model $ model
            & chessPosition .~ position
            & positionTreePath .~ newTreePath
            & showPromotionMenu .~ False
            & forsythEdwards .~ pack (toFEN position)
            & aiData . aiMessage .~ Nothing
        , Event AppSyncBoard
        , Event AppRunAnalysis
        ]
    Node (position, _, _, _) _ = indexTree path _amPositionTree
    path = take _amCurrentPlyNumber newTreePath
    newTreePath = initTreePath <> replicate tailDepth 0
    tailDepth = getTreeTailDepth initTreePath _amPositionTree
    initTreePath = take (_amCurrentPlyNumber-1) _amPositionTreePath <> [v]

undoMoveHandle :: EventHandle
undoMoveHandle model@(AppModel{..}) = response where
    response =
        [ Model $ model
            & chessPosition .~ position
            & positionTree .~ newPositionTree
            & positionTreePath .~ newTreePath
            & currentPlyNumber .~ length newTreePath
            & showPromotionMenu .~ False
            & sanMoves .~ treeToSanMoves newPositionTree
            & forsythEdwards .~ pack (toFEN position)
            & aiData . aiMessage .~ Nothing
        , Event AppSyncBoard
        , Event AppRunAnalysis
        ]
    Node (position, _, _, _) _ = indexTree newTreePath newPositionTree
    newTreePath = initTreePath <> replicate tailDepth 0
    tailDepth = getTreeTailDepth initTreePath newPositionTree
    initTreePath = init _amPositionTreePath
    newPositionTree = pruneTree _amPositionTreePath _amPositionTree

setGameHandle :: Game -> EventHandle
setGameHandle CG{..} model = response where
    response =
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
    producerHandler raiseEvent = loadUciEngine _amUciData $ f raiseEvent
    f raiseEvent event = raiseEvent $ case event of
        EventReportError v -> AppSetErrorMessage $ Just v
        EventSetEngineLoading v -> AppSetEngineLoading v
        EventSetRequestMVars v -> AppSetRequestMVars v
        EventSetCurrentDepth v -> AppSetCurrentEngineDepth v
        EventSetPV v -> AppSetPrincipalVariations v
        EventSetOptionsUCI v -> AppSetOptionsUCI v

setEngineLoadingHandle :: Bool -> EventHandle
setEngineLoadingHandle v model = response where
    response = [Model $ model & uciData . engineLoading .~ v]

setRequestMVarsHandle :: Maybe (MVar String, MVar Position) -> EventHandle
setRequestMVarsHandle v model = [Model $ model & uciData . requestMVars .~ v]

setEngineLogChanHandle :: Maybe (Chan String) -> EventHandle
setEngineLogChanHandle v model = response where
    response = [Model $ model & uciData . engineLogChan .~ v]

setCurrentEngineDepthHandle :: Maybe Text -> EventHandle
setCurrentEngineDepthHandle v model = response where
    response = [Model $ model & uciData . currentEngineDepth .~ v]

setPrincipalVariationsHandle :: [(Text, Maybe Ply)] -> EventHandle
setPrincipalVariationsHandle v model = response where
    response = [Model $ model & uciData . principalVariations .~ v]

setOptionsUCIHandle :: UCIOptions -> EventHandle
setOptionsUCIHandle v model = [Model $ model & uciData . optionsUCI .~ v]

runAnalysisHandle :: EventHandle
runAnalysisHandle model@(AppModel{..}) = response where
    response =
        [ Model $ model & aiData . resetUciBestMove .~ True
        , if _amShowTablebase
            then Producer tablebaseHandler
            else Model $ model & tablebaseData .~ defaultTablebaseData
        , Producer uciHandler
        ]
    tablebaseHandler raiseEvent = do
        raiseEvent $ AppSetTablebaseData $ _amTablebaseData
            { _tbdStatusMessage = Just "Querying..."
            }
        newTablebaseData <- tablebaseRequestAnalysis pos
        raiseEvent $ AppSetTablebaseData newTablebaseData
    uciHandler _ = do
        let (bestMoveVar, bestSyncVar) = fromJust _adUciBestMoveMVar
        when (isJust _adUciBestMoveMVar) $ do
            _ <- tryTakeMVar bestMoveVar
            _ <- tryTakeMVar bestSyncVar
            putMVar bestSyncVar ()
        uciRequestAnalysis _amUciData initPos pos uciMoves
    (initPos, _, _, _) = indexPositionTree model 0
    (pos, _, uciMoves, _) = indexPositionTree model _amCurrentPlyNumber
    AIData{..} = _amAiData

sendEngineRequestHandle :: String -> EventHandle
sendEngineRequestHandle v AppModel{..} = [Producer producerHandler] where
    producerHandler _ = maybe (pure ()) sendRequest _uciRequestMVars
    sendRequest = flip putMVar v . fst
    UCIData{..} = _amUciData

setUciLogsHandle :: Text -> EventHandle
setUciLogsHandle v model = [Model $ model & uciLogs .~ v]

clearUciLogsHandle :: EventHandle
clearUciLogsHandle model@(AppModel{..}) = response where
    response =
        [ Producer producerHandler
        , Model $ model & uciLogs .~ ""
        ]
    producerHandler _ = maybe (pure ()) (flip writeChan msg) _uciEngineLogChan
    msg = "monomer-clear-uci-logs"
    UCIData{..} = _amUciData

recordUCILogsChangedHandle :: Bool -> EventHandle
recordUCILogsChangedHandle v AppModel{..} = [Producer producerHandler] where
    producerHandler _ = maybe (pure ()) (flip writeChan msg) _uciEngineLogChan
    msg = "monomer-record-uci-" <> (show v)
    UCIData{..} = _amUciData

setTablebaseDataHandle :: TablebaseData -> EventHandle
setTablebaseDataHandle v model = [Model $ model & tablebaseData .~ v]
