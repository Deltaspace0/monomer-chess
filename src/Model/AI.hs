{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.AI
    ( ResponseMethod(..)
    , AIData(..)
    , responseMethod
    , uciBestMoveMVar
    , resetUciBestMove
    , mctsRuns
    , minimaxDepth
    , aiMessage
    , initAI
    , calculateMove
    ) where

import Control.Concurrent
import Control.Lens
import Data.Text (Text)
import Game.Chess
import System.Random
import TextShow

import Model.AI.MCTS
import Model.AI.Minimax

data ResponseMethod
    = NoResponse
    | RandomResponse
    | MinimaxResponse
    | MCTSResponse
    | UCIResponse
    deriving Eq

instance Show ResponseMethod where
    show NoResponse = "Do not respond"
    show RandomResponse = "Random moves"
    show MinimaxResponse = "Minimax"
    show MCTSResponse = "Monte Carlo tree search"
    show UCIResponse = "UCI engine"

data AIData = AIData
    { _adResponseMethod :: ResponseMethod
    , _adUciBestMoveMVar :: Maybe (MVar String, MVar ())
    , _adResetUciBestMove :: Bool
    , _adMctsRuns :: Int
    , _adMinimaxDepth :: Int
    , _adAiMessage :: Maybe Text
    } deriving Eq

instance Show AIData where
    show _ = ""

makeLensesWith abbreviatedFields 'AIData

initAI :: AIData
initAI = AIData
    { _adResponseMethod = NoResponse
    , _adUciBestMoveMVar = Nothing
    , _adResetUciBestMove = False
    , _adMctsRuns = 2000
    , _adMinimaxDepth = 4
    , _adAiMessage = Nothing
    }

calculateMove :: Position -> AIData -> IO (Maybe Ply, Maybe Text)
calculateMove pos AIData{..} = result where
    result = case _adResponseMethod of
        NoResponse -> pure (Nothing, Nothing)
        RandomResponse -> onlyPly <$> randomMove pos
        MinimaxResponse -> pure (mmPly, Just $ "Evaluation: " <> showt eval)
        MCTSResponse -> onlyPly <$> mctsMove pos _adMctsRuns
        UCIResponse -> uciMove pos _adUciBestMoveMVar
    (mmPly, eval) = minimaxMove pos _adMinimaxDepth
    onlyPly x = (x, Nothing)

randomMove :: Position -> IO (Maybe Ply)
randomMove position = result where
    result = if null legal
        then pure Nothing
        else Just . (legal!!) <$> randomRIO (0, length legal-1)
    legal = legalPlies position

uciMove
    :: Position
    -> Maybe (MVar String, MVar ())
    -> IO (Maybe Ply, Maybe Text)
uciMove position maybeVar = result where
    result = if null (legalPlies position)
        then return (Nothing, Just "No legal moves")
        else maybe noMVarMessage (getBestMove . fst) maybeVar
    noMVarMessage = return (Nothing, Just "MVar is not initialized")
    getBestMove bestMoveVar = do
        uciText <- takeMVar bestMoveVar
        let ply = fromUCI position uciText
        if null ply && (uciText /= "nouci")
            then getBestMove bestMoveVar
            else return (ply, Nothing)
