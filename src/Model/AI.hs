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

import Control.DeepSeq
import Control.Lens
import Data.Text (Text)
import Game.Chess
import GHC.Generics
import System.Random
import TextShow

import Model.AI.Minimax
import Model.AI.MCTS

data ResponseMethod
    = RandomResponse
    | MinimaxResponse
    | MCTSResponse
    deriving (Eq, Generic)

instance Show ResponseMethod where
    show RandomResponse = "Random moves"
    show MinimaxResponse = "Minimax"
    show MCTSResponse = "Monte Carlo tree search"

data AIData = AIData
    { _adResponseMethod :: ResponseMethod
    , _adMctsRuns :: Int
    , _adMinimaxDepth :: Int
    , _adPositionEvaluation :: Maybe Text
    , _adResponsePly :: Maybe Ply
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
    }

calculateMove :: Position -> AIData -> IO AIData
calculateMove pos aiData@(AIData{..}) = result where
    result = case _adResponseMethod of
        RandomResponse -> randomMove pos <&> \x -> aiData
            { _adPositionEvaluation = Nothing
            , _adResponsePly = x
            }
        MinimaxResponse -> pure $ aiData
            { _adPositionEvaluation = Just $ showt eval
            , _adResponsePly = mmPly
            }
        MCTSResponse -> mctsMove pos _adMctsRuns <&> \x -> aiData
            { _adPositionEvaluation = Nothing
            , _adResponsePly = x
            }
    (mmPly, eval) = minimaxMove pos _adMinimaxDepth

randomMove :: Position -> IO (Maybe Ply)
randomMove position = result where
    result = if null legal
        then pure Nothing
        else Just . (legal!!) <$> randomRIO (0, length legal-1)
    legal = legalPlies position
