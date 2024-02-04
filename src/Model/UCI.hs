{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.UCI
    ( UCIEvent(..)
    , UCIData(..)
    , enginePath
    , engineLoading
    , engineDepth
    , engineLines
    , makeLogs
    , currentEngineDepth
    , principalVariations
    , requestMVar
    , positionMVar
    , defaultUciData
    , loadUciEngine
    , getUciDepth
    , getNewPrincipalVariations
    , uciRequestAnalysis
    ) where

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
import System.Directory
import System.Process
import System.Timeout
import TextShow

data UCIEvent
    = EventReportError Text
    | EventSetEngineLoading Bool
    | EventSetRequestMVar (Maybe (MVar String))
    | EventSetPositionMVar (Maybe (MVar Position))
    | EventSetCurrentDepth (Maybe Text)
    | EventSetPV [Text]
    deriving (Eq, Show)

data UCIData = UCIData
    { _uciEnginePath :: Text
    , _uciEngineLoading :: Bool
    , _uciEngineDepth :: Int
    , _uciEngineLines :: Int
    , _uciMakeLogs :: Bool
    , _uciCurrentEngineDepth :: Maybe Text
    , _uciPrincipalVariations :: [Text]
    , _uciRequestMVar :: Maybe (MVar String)
    , _uciPositionMVar :: Maybe (MVar Position)
    } deriving (Eq, Show)

instance Show (MVar a) where
    show _ = ""

makeLensesWith abbreviatedFields 'UCIData

defaultUciData :: UCIData
defaultUciData = UCIData
    { _uciEnginePath = ""
    , _uciEngineLoading = False
    , _uciEngineDepth = 20
    , _uciEngineLines = 1
    , _uciMakeLogs = False
    , _uciCurrentEngineDepth = Nothing
    , _uciPrincipalVariations = []
    , _uciRequestMVar = Nothing
    , _uciPositionMVar = Nothing
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
    uciTalk (Just hin, Just hout, Just herr, processHandle) = do
        hSetBuffering hin LineBuffering
        hPutStrLn hin "uci"
        mvar <- newEmptyMVar
        rvar <- newEmptyMVar
        pvar <- newEmptyMVar
        depthRef <- newIORef Nothing
        pvRef <- newIORef []
        let setCurrentDepth x = do
                writeIORef depthRef x
                raiseEvent $ EventSetCurrentDepth x
            setPrincipalVariations x = do
                writeIORef pvRef x
                raiseEvent $ EventSetPV x
            readNotEOF f = hIsEOF hout >>= flip unless (hGetLine hout >>= f)
            readErrEOF f = hIsEOF herr >>= flip unless (hGetLine herr >>= f)
            logOutput x = appendFile "logs_uci/outputs.txt" $ x <> "\n"
            logErrorOutput = readErrEOF $ \x -> do
                appendFile "logs_uci/errors.txt" $ x <> "\n"
                logErrorOutput
            waitForUciOk = readNotEOF $ \x -> do
                when _uciMakeLogs $ logOutput x
                if x == "uciok"
                    then putMVar mvar x
                    else waitForUciOk
            uciOutputLoop = hIsEOF hout >>= \eof -> if eof
                then do
                    raiseEvent $ EventSetRequestMVar Nothing
                    raiseEvent $ EventSetPositionMVar Nothing
                    putMVar rvar "eof"
                else do
                    x <- hGetLine hout
                    when _uciMakeLogs $ logOutput x
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
                hPutStrLn hin x
                uciRequestLoop
        when _uciMakeLogs $ do
            createDirectoryIfMissing True "logs_uci"
            _ <- forkIO logErrorOutput
            return ()
        _ <- forkIO waitForUciOk
        uciOutput <- timeout 2000000 $ takeMVar mvar
        raiseEvent $ EventSetEngineLoading False
        if null uciOutput
            then reportError "No support for UCI"
            else do
                unless (null _uciRequestMVar) $ do
                    let rvarOld = fromJust _uciRequestMVar
                    putMVar rvarOld "eof"
                _ <- forkIO uciOutputLoop
                _ <- forkIO uciReportLoop
                raiseEvent $ EventSetRequestMVar $ Just rvar
                raiseEvent $ EventSetPositionMVar $ Just pvar
                uciRequestLoop
                terminateProcess processHandle
    uciTalk _ = reportError "Some IO handles were Nothing"

getUciDepth :: String -> Maybe Text -> Maybe Text
getUciDepth uciOutput depth = newDepth where
    newDepth = if "depth" `elem` ws
        then Just $ pack $ ws!!(fromJust (elemIndex "depth" ws) + 1)
        else depth
    ws = words uciOutput

getNewPrincipalVariations :: Position -> String -> [Text] -> [Text]
getNewPrincipalVariations position uciOutput variations = newVariations where
    newVariations = if "pv" `elem` ws
        then (variations <> emptyLines) & ix (j-1) .~ uciInfo
        else variations
    emptyLines = take (j-length variations) $ repeat "..."
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

uciRequestAnalysis :: UCIData -> Position -> Position -> String -> IO ()
uciRequestAnalysis UCIData{..} initPos pos uciMoves = do
    let rvar = fromJust _uciRequestMVar
        pvar = fromJust _uciPositionMVar
        multiReq = "setoption name MultiPV value " <> (show _uciEngineLines)
        posReq = "position fen " <> (toFEN initPos) <> " moves" <> uciMoves
        goReq = "go depth " <> (show _uciEngineDepth)
    unless (null _uciRequestMVar || null _uciPositionMVar) $ do
        _ <- tryTakeMVar pvar
        putMVar pvar pos
        putMVar rvar "stop"
        putMVar rvar multiReq
        putMVar rvar posReq
        putMVar rvar goReq
