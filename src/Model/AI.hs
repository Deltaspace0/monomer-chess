module Model.AI
    ( randomMove
    , minimaxMove
    ) where

import Data.Function
import Data.List (maximumBy, minimumBy)
import Game.Chess
import System.Random hiding (next)

randomMove :: Position -> StdGen -> (Maybe Ply, StdGen)
randomMove position randomGenerator = (ply, g) where
    ply = if null legal
        then Nothing
        else Just $ legal!!i
    legal = legalPlies position
    (i, g) = randomR (0, length legal-1) randomGenerator

minimaxMove :: Position -> Int -> Maybe Ply
minimaxMove position depth
    | depth <= 0 = Nothing
    | null legal = Nothing
    | otherwise = Just ply
    where
        ply = fst $ minimaxBy (compare `on` f . snd) next
        minimaxBy = if white
            then maximumBy
            else minimumBy
        f x = minimaxEval x depth
        white = color position == White
        next = (\x -> (x, unsafeDoPly position x)) <$> legal
        legal = legalPlies position

minimaxEval :: Position -> Int -> Int
minimaxEval position depth
    | depth <= 0 = evaluatePosition position
    | null next = if white then -1000 else 1000
    | otherwise = if white then maximum evals else minimum evals
    where
        evals = flip minimaxEval (depth-1) <$> next
        next = unsafeDoPly position <$> legalPlies position
        white = color position == White

evaluatePosition :: Position -> Int
evaluatePosition position = sum $ f <$> squares where
    f = evaluatePiece . pieceAt position
    squares =
        [ A8, B8, C8, D8, E8, F8, G8, H8
        , A7, B7, C7, D7, E7, F7, G7, H7
        , A6, B6, C6, D6, E6, F6, G6, H6
        , A5, B5, C5, D5, E5, F5, G5, H5
        , A4, B4, C4, D4, E4, F4, G4, H4
        , A3, B3, C3, D3, E3, F3, G3, H3
        , A2, B2, C2, D2, E2, F2, G2, H2
        , A1, B1, C1, D1, E1, F1, G1, H1
        ]

evaluatePiece :: Maybe (Color, PieceType) -> Int
evaluatePiece Nothing = 0
evaluatePiece (Just (White, Pawn)) = 1
evaluatePiece (Just (White, Knight)) = 3
evaluatePiece (Just (White, Bishop)) = 3
evaluatePiece (Just (White, Rook)) = 5
evaluatePiece (Just (White, Queen)) = 9
evaluatePiece (Just (White, King)) = 1000
evaluatePiece (Just (Black, Pawn)) = -1
evaluatePiece (Just (Black, Knight)) = -3
evaluatePiece (Just (Black, Bishop)) = -3
evaluatePiece (Just (Black, Rook)) = -5
evaluatePiece (Just (Black, Queen)) = -9
evaluatePiece (Just (Black, King)) = -1000
