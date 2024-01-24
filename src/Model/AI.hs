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
    , minimaxEvaluation
    , initAI
    , calculateMove
    ) where

import Control.DeepSeq
import Control.Lens
import Game.Chess
import GHC.Generics
import System.Random

import Model.AI.Minimax
import Model.AI.MCTS

data ResponseMethod
    = RandomResponse
    | MinimaxResponse
    | MCTSResponse
    deriving (Eq, Show, Generic)

data AIData = AIData
    { _adResponseMethod :: ResponseMethod
    , _adMctsRuns :: Int
    , _adMinimaxDepth :: Int
    , _adMinimaxEvaluation :: Maybe Int
    , _adResponsePly :: Maybe Ply
    , _adRandomGenerator :: StdGen
    } deriving (Eq, Show, Generic)

instance NFData Ply where
    rnf x = x `seq` ()

instance NFData ResponseMethod
instance NFData AIData

makeLensesWith abbreviatedFields 'AIData

initAI :: StdGen -> AIData
initAI g = AIData
    { _adResponseMethod = RandomResponse
    , _adMctsRuns = 2000
    , _adMinimaxDepth = 4
    , _adMinimaxEvaluation = Nothing
    , _adResponsePly = Nothing
    , _adRandomGenerator = g
    }

calculateMove :: Position -> AIData -> AIData
calculateMove pos aiData@(AIData{..}) = result where
    result = case _adResponseMethod of
        RandomResponse -> aiData
            { _adMinimaxEvaluation = Nothing
            , _adResponsePly = randomPly
            , _adRandomGenerator = nextRand
            }
        MinimaxResponse -> aiData
            { _adMinimaxEvaluation = Just eval
            , _adResponsePly = mmPly
            }
        MCTSResponse -> aiData
            { _adMinimaxEvaluation = Nothing
            , _adResponsePly = mctsPly
            , _adRandomGenerator = mctsRand
            }
    (randomPly, nextRand) = randomMove pos _adRandomGenerator
    (mmPly, eval) = minimaxMove pos _adMinimaxDepth
    (mctsPly, mctsRand) = mctsMove pos _adRandomGenerator _adMctsRuns

randomMove :: Position -> StdGen -> (Maybe Ply, StdGen)
randomMove position randomGenerator = (ply, g) where
    ply = if null legal
        then Nothing
        else Just $ legal!!i
    legal = legalPlies position
    (i, g) = randomR (0, length legal-1) randomGenerator
