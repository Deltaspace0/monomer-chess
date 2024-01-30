{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.UCI
    ( UCIEvent(..)
    , UCIData(..)
    , enginePath
    , engineDepth
    , engineLines
    , makeLogs
    , principalVariations
    , requestMVar
    , defaultUciData
    , loadUciEngine
    , getNewPrincipalVariations
    , uciRequestAnalysis
    ) where

import Control.Concurrent
import Control.Lens
import Control.Exception
import Control.Monad
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
    = EventOutputReceived String
    | EventReportError Text
    | EventSetRequestMVar (Maybe (MVar String))
    | EventSetPV [Text]
    deriving (Eq, Show)

data UCIData = UCIData
    { _uciEnginePath :: Text
    , _uciEngineDepth :: Int
    , _uciEngineLines :: Int
    , _uciMakeLogs :: Bool
    , _uciPrincipalVariations :: [Text]
    , _uciRequestMVar :: Maybe (MVar String)
    } deriving (Eq, Show)

instance Show (MVar a) where
    show _ = ""

makeLensesWith abbreviatedFields 'UCIData

defaultUciData :: UCIData
defaultUciData = UCIData
    { _uciEnginePath = ""
    , _uciEngineDepth = 20
    , _uciEngineLines = 1
    , _uciMakeLogs = False
    , _uciPrincipalVariations = []
    , _uciRequestMVar = Nothing
    }

loadUciEngine :: UCIData -> (UCIEvent -> IO ()) -> IO ()
loadUciEngine UCIData{..} raiseEvent = loadAction where
    loadAction = do
        let try' = try :: IO a -> IO (Either IOException a)
            path = unpack _uciEnginePath
        processResult <- try' $ createProcess (proc path [])
            { std_out = CreatePipe
            , std_in = CreatePipe
            , std_err = CreatePipe
            }
        case processResult of
            Left _ -> raiseEvent $ EventReportError "Can't run the engine"
            Right (Just hin, Just hout, Just herr, _) -> uciTalk hin hout herr
    uciTalk hin hout herr = do
        hSetBuffering hin LineBuffering
        hPutStrLn hin "uci"
        mvar <- newEmptyMVar
        rvar <- newEmptyMVar
        let reportError = raiseEvent . EventReportError
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
                    putMVar rvar "eof"
                else do
                    x <- hGetLine hout
                    when _uciMakeLogs $ logOutput x
                    raiseEvent $ EventOutputReceived x
                    uciOutputLoop
            uciRequestLoop = takeMVar rvar >>= \x -> unless (x == "eof") $ do
                when ("position" `elem` (words x)) $
                    raiseEvent $ EventSetPV []
                hPutStrLn hin x
                uciRequestLoop
        when _uciMakeLogs $ do
            createDirectoryIfMissing True "logs_uci"
            _ <- forkIO logErrorOutput
            return ()
        _ <- forkIO waitForUciOk
        uciOutput <- timeout 2000000 $ takeMVar mvar
        if null uciOutput
            then reportError "No support for UCI"
            else do
                unless (null _uciRequestMVar) $ do
                    let rvarOld = fromJust _uciRequestMVar
                    putMVar rvarOld "eof"
                _ <- forkIO uciOutputLoop
                raiseEvent $ EventSetRequestMVar $ Just rvar
                uciRequestLoop

getNewPrincipalVariations :: Position -> String -> [Text] -> [Text]
getNewPrincipalVariations position uciOutput variations = newVariations where
    newVariations = if "multipv" `elem` ws
        then (variations <> emptyLines) & ix (j-1) .~ uciInfo
        else variations
    emptyLines = take (j-length variations) $ repeat "..."
    j = read $ ws!!(fromJust (elemIndex "multipv" ws) + 1)
    uciInfo = if and conditions
        then evaluationText <> " " <> movesText
        else "error - invalid UCI"
    conditions =
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

uciRequestAnalysis :: UCIData -> Position -> String -> IO ()
uciRequestAnalysis UCIData{..} position uciMoves = do
    let rvar = fromJust _uciRequestMVar
        multiReq = "setoption name MultiPV value " <> (show _uciEngineLines)
        posReq = "position fen " <> (toFEN position) <> " moves" <> uciMoves
        goReq = "go depth " <> (show _uciEngineDepth)
    unless (null _uciRequestMVar) $ do
        putMVar rvar "stop"
        putMVar rvar multiReq
        putMVar rvar posReq
        putMVar rvar goReq
