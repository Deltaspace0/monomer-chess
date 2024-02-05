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
    , initAI
    , calculateMove
    ) where

import Control.Lens
import Data.Text (Text)
import Game.Chess
import System.Random
import TextShow

import Model.AI.MCTS
import Model.AI.Minimax

data ResponseMethod
    = RandomResponse
    | MinimaxResponse
    | MCTSResponse
    | UCIResponse
    deriving Eq

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
    } deriving (Eq, Show)

makeLensesWith abbreviatedFields 'AIData

initAI :: AIData
initAI = AIData
    { _adResponseMethod = RandomResponse
    , _adMctsRuns = 2000
    , _adMinimaxDepth = 4
    , _adPositionEvaluation = Nothing
    }

calculateMove :: Position -> AIData -> IO (Maybe Ply, Maybe Text)
calculateMove pos AIData{..} = result where
    result = case _adResponseMethod of
        RandomResponse -> onlyPly <$> randomMove pos
        MinimaxResponse -> pure (mmPly, Just $ showt eval)
        MCTSResponse -> onlyPly <$> mctsMove pos _adMctsRuns
        UCIResponse -> pure (Nothing, Nothing)
    (mmPly, eval) = minimaxMove pos _adMinimaxDepth
    onlyPly x = (x, Nothing)

randomMove :: Position -> IO (Maybe Ply)
randomMove position = result where
    result = if null legal
        then pure Nothing
        else Just . (legal!!) <$> randomRIO (0, length legal-1)
    legal = legalPlies position
