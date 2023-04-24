module Model.AI
    ( randomMove
    , minimaxMove
    ) where

import Game.Chess
import System.Random

randomMove :: Position -> StdGen -> (Maybe Ply, StdGen)
randomMove position randomGenerator = (ply, g) where
    ply = if null legal
        then Nothing
        else Just $ legal!!i
    legal = legalPlies position
    (i, g) = randomR (0, length legal-1) randomGenerator

minimaxMove :: Position -> Maybe Ply
minimaxMove _ = Nothing
