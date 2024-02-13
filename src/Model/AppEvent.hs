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
import Data.Sequence (Seq(..))
import Data.Text (pack, unpack, Text)
import Game.Chess
import Game.Chess.SAN
import Monomer
import System.Directory
import TextShow
import qualified Data.Sequence as Seq

import Model.AppModel

data AppEvent
    = AppInit
    | AppSetPosition Position
    | AppSyncBoard
    | AppBoardChanged Bool ([[Piece]], Int, Int)
    | AppEditBoardChanged Bool ([[Piece]], Int, Int)
    | AppSetEditMenu Bool
    | AppSetPromotionMenu Bool
    | AppSetErrorMessage (Maybe Text)
    | AppRunNextPly
    | AppPromote PieceType
    | AppPlayNextResponse
    | AppAbortNextResponse
    | AppResponseCalculated (Maybe Ply, Maybe Text)
    | AppSetResponseThread (Maybe ThreadId)
    | AppPlyNumberChanged Int
    | AppUndoMove
    | AppLoadFEN
    | AppApplyEditChanges
    | AppClearEditBoard
    | AppUpdateFEN
    | AppLoadEngine
    | AppSetEngineLoading Bool
    | AppSetRequestMVars (Maybe (MVar String, MVar Position))
    | AppSetEngineLogChan (Maybe (Chan String))
    | AppSetCurrentEngineDepth (Maybe Text)
    | AppSetPrincipalVariations [Text]
    | AppRunAnalysis
    | AppSendEngineRequest String
    | AppSetUciLogs Text
    | AppClearUciLogs
    | AppRecordUCILogsChanged Bool
    deriving (Eq, Show)

instance NFData Ply where
    rnf x = x `seq` ()

type EventHandle = AppModel -> [AppEventResponse AppModel AppEvent]

handleEvent :: AppEventHandler AppModel AppEvent
handleEvent _ _ model event = case event of
    AppInit -> initHandle model
    AppSetPosition v -> setPositionHandle v model
    AppSyncBoard -> syncBoardHandle model
    AppBoardChanged r info -> boardChangedHandle r info model
    AppEditBoardChanged r info -> editBoardChangedHandle r info model
    AppSetEditMenu v -> setEditMenuHandle v model
    AppSetPromotionMenu v -> setPromotionMenuHandle v model
    AppSetErrorMessage v -> setErrorMessageHandle v model
    AppRunNextPly -> runNextPlyHandle model
    AppPromote pieceType -> promoteHandle pieceType model
    AppPlayNextResponse -> playNextResponseHandle model
    AppAbortNextResponse -> abortNextResponseHandle model
    AppResponseCalculated v -> responseCalculatedHandle v model
    AppSetResponseThread v -> setResponseThreadHandle v model
    AppPlyNumberChanged v -> plyNumberChangedHandle v model
    AppUndoMove -> undoMoveHandle model
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
    AppRunAnalysis -> runAnalysisHandle model
    AppSendEngineRequest v -> sendEngineRequestHandle v model
    AppSetUciLogs v -> setUciLogsHandle v model
    AppClearUciLogs -> clearUciLogsHandle model
    AppRecordUCILogsChanged v -> recordUCILogsChangedHandle v model

initHandle :: EventHandle
initHandle _ = [Producer producerHandler] where
    producerHandler raiseEvent = do
        createDirectoryIfMissing True "logs_uci"
        logChan <- newChan
        raiseEvent $ AppSetEngineLogChan $ Just logChan
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
                doRecord <- readIORef recordRef
                when doRecord $ appendFile "logs_uci/outputs.txt" $ x <> "\n"
                modifyIORef logsListRef $ take 32 . (x:)
                readIORef logsListRef >>= writeIORef logsRef . pack . unlines

setPositionHandle :: Position -> EventHandle
setPositionHandle position model =
    [ Model $ model
        & chessPosition .~ position
        & previousPositions .~ Seq.singleton (position, "", "", "")
        & currentPlyNumber .~ 0
        & showPromotionMenu .~ False
        & sanMoves .~ ""
        & forsythEdwards .~ pack (toFEN position)
        & aiData . positionEvaluation .~ Nothing
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

boardChangedHandle :: Bool -> ([[Piece]], Int, Int) -> EventHandle
boardChangedHandle r info model@(AppModel{..})
    | isJust _amResponseThread = [Event AppSyncBoard]
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
        setNextPly = Model $ model & nextPly .~ Just ply
        ply = getPromotedPly model r info Queen
        noPromotion = null $ plyPromotion ply

editBoardChangedHandle :: Bool -> ([[Piece]], Int, Int) -> EventHandle
editBoardChangedHandle r (_, ixTo, ixFrom) model = response where
    response =
        [ responseIf (ixFrom >= 1000) $ Model newModel
        , Model $ if r
            then interModel & fenData . fenBoardState .~
                reverse (interModel ^. fenData . fenBoardStateReversed)
            else interModel & fenData . fenBoardStateReversed .~
                reverse (interModel ^. fenData . fenBoardState)
        , Event AppUpdateFEN
        ]
    newModel = if r
        then model & fenData . fenBoardStateReversed . element ixTo .~ piece
        else model & fenData . fenBoardState . element ixTo .~ piece
    interModel = if ixFrom >= 1000
        then newModel
        else model
    piece = chessPieces!!(ixFrom-1000)

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

runNextPlyHandle :: EventHandle
runNextPlyHandle model@(AppModel{..}) = response where
    response = if null newPosition || not isLegal
        then []
        else
            [ Model $ model
                & chessPosition .~ fromJust newPosition
                & previousPositions .~ pp :<| cutOffPreviousPositions
                & currentPlyNumber +~ 1
                & showPromotionMenu .~ False
                & sanMoves .~ newSanMoves
                & forsythEdwards .~ newFEN
            , Event AppSyncBoard
            , Event AppRunAnalysis
            ]
    isLegal = (fromJust _amNextPly) `elem` (legalPlies _amChessPosition)
    newPosition = unsafeDoPly _amChessPosition <$> _amNextPly
    pp = (fromJust newPosition, newSanMoves, newUciMoves, san)
    newSanMoves = _amSanMoves <> numberText <> " " <> san
    newFEN = pack $ toFEN $ fromJust newPosition
    numberText = if color _amChessPosition == White
        then (if _amSanMoves == "" then "" else " ") <> number <> "."
        else (if _amSanMoves == "" then "1..." else "")
    number = showt $ moveNumber _amChessPosition
    san = pack $ unsafeToSAN _amChessPosition $ fromJust _amNextPly
    newUciMoves = uciMoves <> " " <> toUCI (fromJust _amNextPly)
    (_, _, uciMoves, _) :<| _ = cutOffPreviousPositions
    cutOffPreviousPositions = Seq.drop i _amPreviousPositions
    i = Seq.length _amPreviousPositions - _amCurrentPlyNumber - 1

promoteHandle :: PieceType -> EventHandle
promoteHandle pieceType model@(AppModel{..}) = response where
    response =
        [ Model $ model & nextPly %~ ((`promoteTo` pieceType) <$>)
        , Event $ AppSetPromotionMenu False
        , Event AppRunNextPly
        , responseIf _amAutoRespond $ Event AppPlayNextResponse
        ]

playNextResponseHandle :: EventHandle
playNextResponseHandle AppModel{..} = response where
    response = [Producer producerHandler]
    producerHandler raiseEvent = do
        mvar <- newEmptyMVar
        thread <- forkIO $ do
            result <- calculateMove _amChessPosition _amAiData
            result `deepseq` pure ()
            raiseEvent $ AppResponseCalculated result
            putMVar mvar ()
        raiseEvent $ AppSetResponseThread $ Just thread
        takeMVar mvar

abortNextResponseHandle :: EventHandle
abortNextResponseHandle model@(AppModel{..}) = response where
    response = if null _amResponseThread
        then []
        else
            [ Model $ model & responseThread .~ Nothing
            , Producer producerHandler
            ]
    producerHandler _ = killThread $ fromJust _amResponseThread

responseCalculatedHandle :: (Maybe Ply, Maybe Text) -> EventHandle
responseCalculatedHandle (responsePly, posEval) model =
    [ Model $ model
        & nextPly .~ responsePly
        & responseThread .~ Nothing
        & aiData . positionEvaluation .~ posEval
    , Event AppRunNextPly
    ]

setResponseThreadHandle :: Maybe ThreadId -> EventHandle
setResponseThreadHandle v model = [Model $ model & responseThread .~ v]

plyNumberChangedHandle :: Int -> EventHandle
plyNumberChangedHandle newPlyNumber model@(AppModel{..}) = response where
    response = if newPlyNumber < 0 || newPlyNumber > l-1
        then []
        else
            [ Model $ model
                & chessPosition .~ previousPosition
                & currentPlyNumber .~ newPlyNumber
                & showPromotionMenu .~ False
                & sanMoves .~ moves
                & forsythEdwards .~ pack (toFEN previousPosition)
                & aiData . positionEvaluation .~ Nothing
            , Event AppSyncBoard
            , Event AppRunAnalysis
            ]
    (previousPosition, moves, _, _) = fromJust lookupResult
    lookupResult = Seq.lookup (l-newPlyNumber-1) _amPreviousPositions
    l = Seq.length _amPreviousPositions

undoMoveHandle :: EventHandle
undoMoveHandle model@(AppModel{..}) = response where
    response =
        [ Model $ model
            & chessPosition .~ previousPosition
            & previousPositions .~ Seq.drop 1 _amPreviousPositions
            & currentPlyNumber .~ Seq.length _amPreviousPositions-2
            & showPromotionMenu .~ False
            & sanMoves .~ moves
            & forsythEdwards .~ pack (toFEN previousPosition)
            & aiData . positionEvaluation .~ Nothing
        , Event AppSyncBoard
        , Event AppRunAnalysis
        ]
    _ :<| (previousPosition, moves, _, _) :<| _ = _amPreviousPositions

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
        & fenData . fenBoardState .~ take 64 (repeat [])
        & fenData . fenBoardStateReversed .~ take 64 (repeat [])
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

setPrincipalVariationsHandle :: [Text] -> EventHandle
setPrincipalVariationsHandle v model = response where
    response = [Model $ model & uciData . principalVariations .~ v]

runAnalysisHandle :: EventHandle
runAnalysisHandle AppModel{..} = [Producer producerHandler] where
    producerHandler _ = uciRequestAnalysis _amUciData initPos pos uciMoves
    _ :|> (initPos, _, _, _) = _amPreviousPositions
    (pos, _, uciMoves, _) = fromJust $ Seq.lookup i _amPreviousPositions
    i = Seq.length _amPreviousPositions - _amCurrentPlyNumber - 1

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
