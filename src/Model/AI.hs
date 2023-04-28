module Model.AI
    ( module Model.AI.Minimax
    , randomMove
    ) where

import Game.Chess
import System.Random

import Model.AI.Minimax

randomMove :: Position -> StdGen -> (Maybe Ply, StdGen)
randomMove position randomGenerator = (ply, g) where
    ply = if null legal
        then Nothing
        else Just $ legal!!i
    legal = legalPlies position
    (i, g) = randomR (0, length legal-1) randomGenerator
