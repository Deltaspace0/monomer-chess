{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.UCI
    ( module Model.UCIOptions
    , UCILog(..)
    , UCIEvent(..)
    , UCIData(..)
    , engineIndex
    , engineNextIndex
    , engineLiveReport
    , enginePath
    , engineLoading
    , engineDepth
    , engineNodes
    , engineDepthOrNodes
    , currentEngineDepth
    , principalVariations
    , requestMVars
    , bestMoveMVars
    , engineLogChan
    , optionsUCI
    , defaultUciData
    , loadUciEngine
    , getUciDepth
    , getUciBestMove
    , getNewPrincipalVariations
    , uciRequestAnalysis
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Lens
import Control.Exception
import Control.Monad
import Data.IORef
import Data.List (elemIndex)
import Data.Maybe
import Data.Text (pack, unpack, Text)
import Game.Chess
import Game.Chess.SAN
import Numeric
import System.IO
import System.Process
import System.Timeout
import TextShow

import Model.UCIOptions

data UCILog
    = LogInput Int String
    | LogOutput Int String
    | LogError Int String
    | LogDoRecord Bool
    | LogClear
    deriving (Eq, Show)

data UCIEvent
    = EventReportError Text
    | EventSetEngineLoading Bool
    | EventSetRequestMVars (Maybe (MVar String, MVar Position))
    | EventSetBestMoveMVars (Maybe (MVar String, MVar ()))
    | EventSetCurrentDepth (Maybe Text)
    | EventSetPV [(Text, Maybe Ply, Maybe Double)]
    | EventSetOptionsUCI UCIOptions
    deriving (Eq, Show)

data UCIData = UCIData
    { _uciEngineIndex :: Int
    , _uciEngineNextIndex :: Int
    , _uciEngineLiveReport :: Bool
    , _uciEnginePath :: Text
    , _uciEngineLoading :: Bool
    , _uciEngineDepth :: Int
    , _uciEngineNodes :: Int
    , _uciEngineDepthOrNodes :: Bool
    , _uciCurrentEngineDepth :: Maybe Text
    , _uciPrincipalVariations :: [(Text, Maybe Ply, Maybe Double)]
    , _uciRequestMVars :: Maybe (MVar String, MVar Position)
    , _uciBestMoveMVars :: Maybe (MVar String, MVar ())
    , _uciEngineLogChan :: Maybe (Chan UCILog)
    , _uciOptionsUCI :: UCIOptions
    } deriving (Eq, Show)

instance Show (MVar a) where
    show _ = ""

instance Show (Chan a) where
    show _ = ""

makeLensesWith abbreviatedFields 'UCIData

defaultUciData :: UCIData
defaultUciData = UCIData
    { _uciEngineIndex = 0
    , _uciEngineNextIndex = 0
    , _uciEngineLiveReport = True
    , _uciEnginePath = ""
    , _uciEngineLoading = False
    , _uciEngineDepth = 20
    , _uciEngineNodes = 500000
    , _uciEngineDepthOrNodes = True
    , _uciCurrentEngineDepth = Nothing
    , _uciPrincipalVariations = []
    , _uciRequestMVars = Nothing
    , _uciBestMoveMVars = Nothing
    , _uciEngineLogChan = Nothing
    , _uciOptionsUCI = initUciOptions []
    }

loadUciEngine :: UCIData -> (UCIEvent -> IO ()) -> IO ()
loadUciEngine UCIData{..} raiseEvent = loadAction where
    loadAction = do
        raiseEvent $ EventSetEngineLoading True
        processResult <- try' $ createProcess (proc path [])
            { std_out = CreatePipe
            , std_in = CreatePipe
            , std_err = CreatePipe
            }
        case processResult of
            Left _ -> do
                raiseEvent $ EventSetEngineLoading False
                reportError "Can't run the engine"
            Right processHandles -> uciTalk processHandles
    try' = try :: IO a -> IO (Either IOException a)
    path = unpack _uciEnginePath
    reportError = raiseEvent . EventReportError
    logChan = fromJust _uciEngineLogChan
    logsEnabled = isJust _uciEngineLogChan
    uciTalk (Just hin, Just hout, Just herr, processHandle) = do
        mvar <- newEmptyMVar
        rvar <- newEmptyMVar
        pvar <- newEmptyMVar
        bestMoveVar <- newEmptyMVar
        bestSyncVar <- newEmptyMVar
        depthRef <- newIORef Nothing
        pvRef <- newIORef []
        optRef <- newIORef []
        let bestVars = (bestMoveVar, bestSyncVar)
            putLine x = do
                hPutStrLn hin x
                let msg = LogInput _uciEngineIndex x
                when logsEnabled $ writeChan logChan msg
            outGetLine = do
                x <- hGetLine hout
                let msg = LogOutput _uciEngineIndex x
                when logsEnabled $ writeChan logChan msg
                return x
            setCurrentDepth x = do
                writeIORef depthRef x
                raiseEvent $ EventSetCurrentDepth x
            setPrincipalVariations x = do
                writeIORef pvRef x
                raiseEvent $ EventSetPV x
            readNotEOF f = hIsEOF hout >>= flip unless (outGetLine >>= f)
            readErrEOF f = hIsEOF herr >>= flip unless (hGetLine herr >>= f)
            logErrorOutput = readErrEOF $ \x -> do
                writeChan logChan $ LogError _uciEngineIndex x
                logErrorOutput
            waitForUciOk = readNotEOF $ \x -> do
                let opt = parseUciOption x
                when ("option" `elem` words x) $
                    atomicModifyIORef optRef $ \v -> (maybe v (:v) opt, ())
                if x == "uciok"
                    then putMVar mvar x
                    else waitForUciOk
            uciOutputLoop = hIsEOF hout >>= \eof -> if eof
                then do
                    raiseEvent $ EventSetRequestMVars Nothing
                    raiseEvent $ EventSetBestMoveMVars Nothing
                    putMVar rvar "eof"
                else do
                    x <- outGetLine
                    pos <- readMVar pvar
                    let getNewPV = getNewPrincipalVariations pos
                        uciBestMove = getUciBestMove x
                    when (isJust uciBestMove) $ do
                        _ <- tryTakeMVar bestMoveVar
                        putMVar bestMoveVar $ fromJust uciBestMove
                    atomicModifyIORef depthRef $ \v -> (getUciDepth x v, ())
                    atomicModifyIORef pvRef $ \v -> (getNewPV x v, ())
                    uciOutputLoop
            uciReportLoop = forever $ do
                threadDelay 200000
                readIORef depthRef >>= raiseEvent . EventSetCurrentDepth
                readIORef pvRef >>= raiseEvent . EventSetPV
            uciRequestLoop = takeMVar rvar >>= \x -> unless (x == "eof") $ do
                when ("position" `elem` (words x)) $ do
                    setCurrentDepth Nothing
                    setPrincipalVariations []
                if x == "report-pv"
                    then readIORef pvRef >>= raiseEvent . EventSetPV
                    else putLine x
                uciRequestLoop
        hSetBuffering hin LineBuffering
        putLine "uci"
        when logsEnabled $ forkIO logErrorOutput >> return ()
        _ <- forkIO waitForUciOk
        uciOutput <- timeout 2000000 $ takeMVar mvar
        raiseEvent $ EventSetEngineLoading False
        if null uciOutput
            then reportError "No support for UCI"
            else do
                unless (null _uciRequestMVars) $ do
                    let rvarOld = fst $ fromJust _uciRequestMVars
                    putMVar rvarOld "eof"
                _ <- forkIO uciOutputLoop
                reportThread <- if _uciEngineLiveReport
                    then Just <$> forkIO uciReportLoop
                    else return Nothing
                opts <- initUciOptions . reorderUciOpts <$> readIORef optRef
                raiseEvent $ EventSetRequestMVars $ Just (rvar, pvar)
                raiseEvent $ EventSetBestMoveMVars $ Just bestVars
                raiseEvent $ EventSetOptionsUCI opts
                uciRequestLoop
                terminateProcess processHandle
                when (isJust reportThread) $
                    killThread $ fromJust reportThread
                raiseEvent $ EventSetCurrentDepth Nothing
                raiseEvent $ EventSetPV []
                raiseEvent $ EventSetRequestMVars Nothing
                raiseEvent $ EventSetBestMoveMVars Nothing
    uciTalk _ = reportError "Some IO handles were Nothing"

reorderUciOpts :: [OptionUCI] -> [OptionUCI]
reorderUciOpts opts = commonOpts <> otherOpts where
    (commonOpts, otherOpts) = processOpts opts
    processOpts [] = ([], [])
    processOpts (x:xs) = let (a, b) = processOpts xs in case replaceOpt x of
        Left opt -> (opt:a, b)
        Right opt -> (a, opt:b)
    replaceOpt x = case x of
        SpinUCI "MultiPV" _ _ _ -> Left $ x
            { _spuSpinMinValue = 1
            , _spuSpinMaxValue = 10
            }
        SpinUCI "Threads" _ _ _ -> Left $ x
            { _spuSpinMinValue = 1
            , _spuSpinMaxValue = 10
            }
        SpinUCI "Skill Level" _ _ _ -> Left x
        _ -> Right x

getUciDepth :: String -> Maybe Text -> Maybe Text
getUciDepth uciOutput depth = newDepth <|> depth where
    newDepth = (pack . (ws!!) . succ) <$> elemIndex "depth" ws
    ws = words uciOutput

getUciBestMove :: String -> Maybe String
getUciBestMove uciOutput = ((ws!!) . succ) <$> elemIndex "bestmove" ws where
    ws = words uciOutput

getNewPrincipalVariations
    :: Position
    -> String
    -> [(Text, Maybe Ply, Maybe Double)]
    -> [(Text, Maybe Ply, Maybe Double)]
getNewPrincipalVariations position uciOutput variations = newVariations where
    newVariations = if "score" `elem` ws
        then (variations <> emptyLines) & ix (j-1) .~ newVariation
        else variations
    emptyLines = replicate (j-length variations) ("...", Nothing, Nothing)
    j = if "multipv" `elem` ws
        then read $ ws!!(fromJust (elemIndex "multipv" ws) + 1)
        else 1
    newVariation = (uciInfo, firstPly, evalDouble)
    uciInfo
        | not correctUci = ""
        | correctUci && movesText == "???" = ""
        | correctUci = evaluationText <> " " <> movesText
        | otherwise = "error - invalid UCI"
    correctUci = and
        [ "score" `elem` ws
        , "pv" `elem` ws
        , fromJust si <= length ws-3
        ]
    evaluationText = case evalType of
        Just "cp" -> (if signCondition then "-" else "+") <> cpNumberText
        Just "mate" -> (if signCondition then "#-" else "#") <> mateNumberText
        _ -> "???"
    evalDouble = case evalType of
        Just "cp" | signCondition -> Just $ -(abs cpNumber)
        Just "cp" -> Just $ abs cpNumber
        Just "mate" | signCondition -> Just (-200)
        Just "mate" -> Just 200
        _ -> Nothing
    signCondition = (color position == White) /= (cpNumber > 0)
    mateNumberText = showt $ abs (read evalNumber :: Int)
    cpNumberText = pack $ showFFloat Nothing (abs cpNumber) ""
    cpNumber = (read evalNumber :: Double)/100
    evalType = (\x -> ws!!(x+1)) <$> si
    evalNumber = ws!!(fromJust si + 2)
    movesText = snd $ foldl foldUCI (Just position, "") $ fromJust uciMoves
    foldUCI (Nothing, _) _ = (Nothing, "???")
    foldUCI (Just pos, sanMoves) uciMove = res where
        res = if null ply || (not $ (fromJust ply) `elem` legalPlies pos)
            then (Nothing, "???")
            else (newPos, newSanMoves)
        newPos = unsafeDoPly pos <$> ply
        newSanMoves = sanMoves <> numberText <> " " <> san
        numberText = if color pos == White
            then (if sanMoves == "" then "" else " ") <> number <> "."
            else (if sanMoves == "" then number <> "..." else "")
        number = showt $ moveNumber pos
        san = pack $ unsafeToSAN pos $ fromJust ply
        ply = fromUCI pos uciMove
    uciMoves = flip drop ws . succ <$> elemIndex "pv" ws
    si = elemIndex "score" ws
    ws = words uciOutput
    firstPly = uciMoves >>= listToMaybe >>= fromUCI position

uciRequestAnalysis :: UCIData -> Position -> Position -> String -> IO ()
uciRequestAnalysis UCIData{..} initPos pos uciMoves = do
    let (rvar, pvar) = fromJust _uciRequestMVars
        posReq = "position fen " <> (toFEN initPos) <> " moves" <> uciMoves
        goReq = if _uciEngineDepthOrNodes
            then "go depth " <> (show _uciEngineDepth)
            else "go nodes " <> (show _uciEngineNodes)
    unless (null _uciRequestMVars) $ do
        _ <- tryTakeMVar pvar
        putMVar pvar pos
        putMVar rvar "stop"
        putMVar rvar posReq
        putMVar rvar goReq
