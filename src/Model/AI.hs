{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.AI
    ( ResponseMethod(..)
    , AIData(..)
    , responseMethod
    , mctsRuns
    , minimaxDepth
    , positionEvaluation
    , uciEnginePath
    , uciEngineDepth
    , aiMessage
    , initAI
    , calculateMove
    ) where

import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Control.Lens
import Control.Monad
import Data.List
import Data.Maybe
import Data.Text (Text, pack, unpack)
import Game.Chess
import GHC.Generics
import System.IO
import System.Process
import System.Random
import System.Timeout
import TextShow

import Model.AI.Minimax
import Model.AI.MCTS

data ResponseMethod
    = RandomResponse
    | MinimaxResponse
    | MCTSResponse
    | UCIResponse
    deriving (Eq, Generic)

instance Show ResponseMethod where
    show RandomResponse = "Random moves"
    show MinimaxResponse = "Minimax"
    show MCTSResponse = "Monte Carlo tree search"
    show UCIResponse = "UCI engine"

data AIData = AIData
    { _adResponseMethod :: ResponseMethod
    , _adMctsRuns :: Int
    , _adMinimaxDepth :: Int
    , _adPositionEvaluation :: Maybe Text
    , _adResponsePly :: Maybe Ply
    , _adAiMessage :: Maybe Text
    , _adUciEnginePath :: Text
    , _adUciEngineDepth :: Int
    } deriving (Eq, Show, Generic)

instance NFData Ply where
    rnf x = x `seq` ()

instance NFData ResponseMethod
instance NFData AIData

makeLensesWith abbreviatedFields 'AIData

initAI :: AIData
initAI = AIData
    { _adResponseMethod = RandomResponse
    , _adMctsRuns = 2000
    , _adMinimaxDepth = 4
    , _adPositionEvaluation = Nothing
    , _adResponsePly = Nothing
    , _adAiMessage = Nothing
    , _adUciEnginePath = ""
    , _adUciEngineDepth = 20
    }

calculateMove :: Position -> AIData -> IO AIData
calculateMove pos aiData@(AIData{..}) = result where
    result = case _adResponseMethod of
        RandomResponse -> randomMove pos <&> \x -> cleanData
            & responsePly .~ x
        MinimaxResponse -> pure $ cleanData
            & positionEvaluation .~ Just (showt eval)
            & responsePly .~ mmPly
        MCTSResponse -> mctsMove pos _adMctsRuns <&> \x -> cleanData
            & responsePly .~ x
        UCIResponse -> uciResponse <&> \(uciPly, uciEval, msg) -> cleanData
            & positionEvaluation .~ uciEval
            & responsePly .~ uciPly
            & aiMessage .~ msg
    (mmPly, eval) = minimaxMove pos _adMinimaxDepth
    uciResponse = uciMove pos _adUciEngineDepth _adUciEnginePath
    cleanData = aiData
        & positionEvaluation .~ Nothing
        & aiMessage .~ Nothing

randomMove :: Position -> IO (Maybe Ply)
randomMove position = result where
    result = if null legal
        then pure Nothing
        else Just . (legal!!) <$> randomRIO (0, length legal-1)
    legal = legalPlies position

uciMove :: Position -> Int -> Text -> IO (Maybe Ply, Maybe Text, Maybe Text)
uciMove position depth path = result where
    result = uciProcess >>= \processResult -> case processResult of
        Left _ -> return $ msg "Can't run the engine"
        Right (Just hin, Just hout, _, _) -> uciTalk hin hout
    uciTalk hin hout = do
        hSetBuffering hin NoBuffering
        hPutStrLn hin "uci"
        mvar <- newEmptyMVar
        evar <- newEmptyMVar
        let readNotEOF f = hIsEOF hout >>= flip unless (hGetLine hout >>= f)
        let waitForUciOk = readNotEOF $ \x -> if x == "uciok"
                then putMVar mvar x
                else waitForUciOk
        let waitForBestMove = hIsEOF hout >>= \eof -> if eof
                then putMVar mvar ""
                else hGetLine hout >>= \x -> do
                    let ws = words x
                    when ("score" `elem` ws) $ do
                        _ <- tryTakeMVar evar
                        putMVar evar ws
                    if head ws == "bestmove"
                        then putMVar mvar $ ws!!1
                        else waitForBestMove
        _ <- forkIO waitForUciOk
        response <- timeout 2000000 $ takeMVar mvar
        if null response then return $ msg "No support for UCI" else do
            hPutStrLn hin $ "position fen " <> toFEN position
            hPutStrLn hin $ "go depth " <> show depth
            _ <- forkIO waitForBestMove
            uciNotation <- takeMVar mvar
            uciEval <- (extractUciEval position <$>) <$> tryTakeMVar evar
            return (fromUCI position uciNotation, uciEval, Nothing)
    uciProcess = try' $ createProcess (proc (unpack path) [])
        { std_out = CreatePipe
        , std_in = CreatePipe
        , std_err = CreatePipe
        }
    try' = try :: IO a -> IO (Either IOException a)
    msg x = (Nothing, Nothing, Just x)

extractUciEval :: Position -> [String] -> Text
extractUciEval position ws
    | s > length ws-3 = "error - invalid UCI"
    | evalType == "cp" = c <> " has advantage of " <> cpNumber
    | evalType == "mate" = c <> " mates in " <> mateNumberText <> " moves"
    | otherwise = "error - unknown score type"
    where
        mateNumberText = showt $ abs mateNumber
        mateNumber = read evalNumber :: Int
        cpNumber = showt $ abs $ (read evalNumber :: Double)/100
        evalType = ws!!(s+1)
        evalNumber = ws!!(s+2)
        s = fromJust $ elemIndex "score" ws
        c = pack $ show $ if mateNumber > 0
            then color position
            else opponent $ color position
