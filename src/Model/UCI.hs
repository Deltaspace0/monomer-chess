{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.UCI
    ( module Model.UCIOptions
    , UCIEvent(..)
    , UCIData(..)
    , enginePath
    , engineLoading
    , engineDepth
    , currentEngineDepth
    , principalVariations
    , requestMVars
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

data UCIEvent
    = EventReportError Text
    | EventSetEngineLoading Bool
    | EventSetRequestMVars (Maybe (MVar String, MVar Position))
    | EventSetCurrentDepth (Maybe Text)
    | EventSetPV [(Text, Maybe Ply)]
    | EventSetOptionsUCI UCIOptions
    deriving (Eq, Show)

data UCIData = UCIData
    { _uciEnginePath :: Text
    , _uciEngineLoading :: Bool
    , _uciEngineDepth :: Int
    , _uciCurrentEngineDepth :: Maybe Text
    , _uciPrincipalVariations :: [(Text, Maybe Ply)]
    , _uciRequestMVars :: Maybe (MVar String, MVar Position)
    , _uciEngineLogChan :: Maybe (Chan String)
    , _uciOptionsUCI :: UCIOptions
    } deriving (Eq, Show)

instance Show (MVar a) where
    show _ = ""

instance Show (Chan a) where
    show _ = ""

makeLensesWith abbreviatedFields 'UCIData

defaultUciData :: UCIData
defaultUciData = UCIData
    { _uciEnginePath = ""
    , _uciEngineLoading = False
    , _uciEngineDepth = 20
    , _uciCurrentEngineDepth = Nothing
    , _uciPrincipalVariations = []
    , _uciRequestMVars = Nothing
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
        depthRef <- newIORef Nothing
        pvRef <- newIORef []
        optRef <- newIORef []
        let putLine x = do
                hPutStrLn hin x
                when logsEnabled $ writeChan logChan $ "stdin: " <> x
            outGetLine = do
                x <- hGetLine hout
                when logsEnabled $ writeChan logChan $ "stdout: " <> x
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
                writeChan logChan $ "stderr: " <> x
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
                    putMVar rvar "eof"
                else do
                    x <- outGetLine
                    pos <- readMVar pvar
                    let getNewPV = getNewPrincipalVariations pos
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
                putLine x
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
                reportThread <- forkIO uciReportLoop
                opts <- initUciOptions . reorderUciOpts <$> readIORef optRef
                raiseEvent $ EventSetRequestMVars $ Just (rvar, pvar)
                raiseEvent $ EventSetOptionsUCI opts
                uciRequestLoop
                terminateProcess processHandle
                killThread reportThread
                raiseEvent $ EventSetCurrentDepth Nothing
                raiseEvent $ EventSetPV []
                raiseEvent $ EventSetRequestMVars Nothing
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
    -> [(Text, Maybe Ply)]
    -> [(Text, Maybe Ply)]
getNewPrincipalVariations position uciOutput variations = newVariations where
    newVariations = if "pv" `elem` ws
        then (variations <> emptyLines) & ix (j-1) .~ (uciInfo, firstPly)
        else variations
    emptyLines = replicate (j-length variations) ("...", Nothing)
    j = if "multipv" `elem` ws
        then read $ ws!!(fromJust (elemIndex "multipv" ws) + 1)
        else 1
    uciInfo
        | correctUci && movesText == "???" = ""
        | correctUci = evaluationText <> " " <> movesText
        | otherwise = "error - invalid UCI"
    correctUci = and
        [ "score" `elem` ws
        , "pv" `elem` ws
        , si <= length ws-3
        ]
    evaluationText = case evalType of
        "cp" -> (if signCondition then "-" else "+") <> cpNumberText
        "mate" -> (if signCondition then "#-" else "#") <> mateNumberText
        _ -> "???"
    signCondition = (color position == White) /= (cpNumber > 0)
    mateNumberText = showt $ abs (read evalNumber :: Int)
    cpNumberText = pack $ showFFloat Nothing (abs cpNumber) ""
    cpNumber = (read evalNumber :: Double)/100
    evalType = ws!!(si+1)
    evalNumber = ws!!(si+2)
    movesText = snd $ foldl foldUCI (Just position, "") uciMoves
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
    uciMoves = drop (fromJust (elemIndex "pv" ws) + 1) ws
    si = fromJust $ elemIndex "score" ws
    ws = words uciOutput
    firstPly = listToMaybe uciMoves >>= fromUCI position

uciRequestAnalysis :: UCIData -> Position -> Position -> String -> IO ()
uciRequestAnalysis UCIData{..} initPos pos uciMoves = do
    let (rvar, pvar) = fromJust _uciRequestMVars
        posReq = "position fen " <> (toFEN initPos) <> " moves" <> uciMoves
        goReq = "go depth " <> (show _uciEngineDepth)
    unless (null _uciRequestMVars) $ do
        _ <- tryTakeMVar pvar
        putMVar pvar pos
        putMVar rvar "stop"
        putMVar rvar posReq
        putMVar rvar goReq
