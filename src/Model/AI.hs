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
    , aiMessage
    , initAI
    , calculateMove
    ) where

import Control.DeepSeq
import Control.Lens
import Data.Text (Text)
import Game.Chess
import GHC.Generics
import System.Random
import TextShow

import Model.AI.MCTS
import Model.AI.Minimax

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
        UCIResponse -> pure $ cleanData
            & responsePly .~ Nothing
    (mmPly, eval) = minimaxMove pos _adMinimaxDepth
    cleanData = aiData
        & positionEvaluation .~ Nothing
        & aiMessage .~ Nothing

randomMove :: Position -> IO (Maybe Ply)
randomMove position = result where
    result = if null legal
        then pure Nothing
        else Just . (legal!!) <$> randomRIO (0, length legal-1)
    legal = legalPlies position
