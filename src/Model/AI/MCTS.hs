module Model.AI.MCTS
    ( mctsMove
    ) where

import Game.Chess
import System.Random

mctsMove :: Position -> StdGen -> (Maybe Ply, StdGen)
mctsMove _ randomGenerator = (Nothing, randomGenerator)
