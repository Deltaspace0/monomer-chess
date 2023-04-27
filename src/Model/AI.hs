module Model.AI
    ( randomMove
    , minimaxMove
    ) where

import Data.Function
import Data.List (maximumBy, minimumBy)
import Game.Chess
import System.Random hiding (next)

data PositionEval = PositionEval Position Int

randomMove :: Position -> StdGen -> (Maybe Ply, StdGen)
randomMove position randomGenerator = (ply, g) where
    ply = if null legal
        then Nothing
        else Just $ legal!!i
    legal = legalPlies position
    (i, g) = randomR (0, length legal-1) randomGenerator

minimaxMove :: Position -> Int -> (Maybe Ply, Int)
minimaxMove position depth
    | depth <= 0 = (Nothing, 0)
    | null legal = (Nothing, 0)
    | otherwise = (Just ply, eval)
    where
        (ply, eval) = minimaxBy (compare `on` snd) next
        minimaxBy = if white
            then maximumBy
            else minimumBy
        white = color position == White
        next = (\x -> (x, f $ doPlyEval positionEval x)) <$> legal
        f x = alphabeta x depth (-100000) 100000
        positionEval = makePositionEval position
        legal = legalPlies position

alphabeta :: PositionEval -> Int -> Int -> Int -> Int
alphabeta positionEval@(PositionEval p eval) depth a b
    | depth <= 0 = eval
    | null nextPositionEvals = v
    | otherwise = f nextPositionEvals depth a b v
    where
        v = if white then -1000 else 1000
        f = if white then alphabetaMaxi else alphabetaMini
        nextPositionEvals = doPlyEval positionEval <$> legalPlies p
        white = color p == White

alphabetaMaxi :: [PositionEval] -> Int -> Int -> Int -> Int -> Int
alphabetaMaxi [] _ _ _ v = v
alphabetaMaxi (x:xs) depth a b v = result where
    result = if value >= b
        then value
        else alphabetaMaxi xs depth (max a value) b value
    value = max v $ alphabeta x (depth-1) a b

alphabetaMini :: [PositionEval] -> Int -> Int -> Int -> Int -> Int
alphabetaMini [] _ _ _ v = v
alphabetaMini (x:xs) depth a b v = result where
    result = if value <= a
        then value
        else alphabetaMini xs depth a (min b value) value
    value = min v $ alphabeta x (depth-1) a b

makePositionEval :: Position -> PositionEval
makePositionEval p = PositionEval p $ evaluatePosition p

doPlyEval :: PositionEval -> Ply -> PositionEval
doPlyEval (PositionEval p eval) ply = newPositionEval where
    newPositionEval = PositionEval (unsafeDoPly p ply) newEval
    newEval = eval-capturedEval+enPassantEval
    capturedEval = evaluatePiece $ pieceAt p $ plyTarget ply
    enPassantEval
        | not ep = 0
        | color p == White = 1
        | otherwise = -1
    ep = sourcePawn && enPassantSquare p == Just (plyTarget ply)
    sourcePawn = sp `elem` [Just (White, Pawn), Just (Black, Pawn)]
    sp = pieceAt p $ plySource ply

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
