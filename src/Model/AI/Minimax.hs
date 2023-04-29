module Model.AI.Minimax
    ( minimaxMove
    ) where

import Data.List (sortOn)
import Game.Chess

import Model.AI.PositionEval

minimaxMove :: Position -> Int -> (Maybe Ply, Int)
minimaxMove position depth = result where
    result = alphabeta positionEval depth (-1000) 1000
    positionEval = makePositionEval position

alphabeta :: PositionEval -> Int -> Int -> Int -> (Maybe Ply, Int)
alphabeta positionEval@(PositionEval p eval) depth a b = r where
    r = if depth <= 0 || null nextPPE
        then (Nothing, eval)
        else f nextPPE depth a b v
    v = (Nothing, if white then -100000 else 100000)
    f = if white then alphabetaMaxi else alphabetaMini
    nextPPE = sortOn forcingIndicator $ doPlies <$> legalPlies p
    forcingIndicator :: (Ply, PositionEval) -> Int
    forcingIndicator (ply, PositionEval nextPosition _)
        | inCheck (color nextPosition) nextPosition = 0
        | not (null $ plyPromotion ply) = 1
        | not (null $ pieceAt p $ plyTarget ply) = 2
        | otherwise = 3
    doPlies x = (x, doPlyEval positionEval x)
    white = color p == White

alphabetaMaxi
    :: [(Ply, PositionEval)]
    -> Int
    -> Int
    -> Int
    -> (Maybe Ply, Int)
    -> (Maybe Ply, Int)
alphabetaMaxi [] _ _ _ v = v
alphabetaMaxi ((ply', pe):xs) depth a b v@(_, e) = result where
    result = if endEval > b
        then value
        else alphabetaMaxi xs depth a' b value
    a' = max a endEval
    value@(_, endEval) = if e >= abEval
        then v
        else (Just ply', abEval)
    abEval = snd $ alphabeta pe (depth-1) a b

alphabetaMini
    :: [(Ply, PositionEval)]
    -> Int
    -> Int
    -> Int
    -> (Maybe Ply, Int)
    -> (Maybe Ply, Int)
alphabetaMini [] _ _ _ v = v
alphabetaMini ((ply', pe):xs) depth a b v@(_, e) = result where
    result = if endEval < a
        then value
        else alphabetaMini xs depth a b' value
    b' = min b endEval
    value@(_, endEval) = if e <= abEval
        then v
        else (Just ply', abEval)
    abEval = snd $ alphabeta pe (depth-1) a b
