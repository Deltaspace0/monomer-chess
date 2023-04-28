module Model.AI
    ( randomMove
    , minimaxMove
    ) where

import Data.List (sortOn)
import Game.Chess
import System.Random

data PositionEval = PositionEval Position Int

randomMove :: Position -> StdGen -> (Maybe Ply, StdGen)
randomMove position randomGenerator = (ply, g) where
    ply = if null legal
        then Nothing
        else Just $ legal!!i
    legal = legalPlies position
    (i, g) = randomR (0, length legal-1) randomGenerator

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

makePositionEval :: Position -> PositionEval
makePositionEval p = PositionEval p eval where
    eval = evaluatePosition p + checkmateFactor p

doPlyEval :: PositionEval -> Ply -> PositionEval
doPlyEval (PositionEval p eval) ply = newPositionEval where
    newPositionEval = PositionEval newPosition $ eval+(sum evals)
    evals =
        [ -(evaluatePiece $ pieceAt p $ plyTarget ply)
        , checkmateFactor newPosition
        , promotionEval
        , enPassantEval
        ]
    promotionEval = if null promotedPiece
        then 0
        else evaluatePiece promotedPiece - evaluatePiece sp
    promotedPiece = (\x -> (color p, x)) <$> plyPromotion ply
    enPassantEval
        | not ep = 0
        | color p == White = 1
        | otherwise = -1
    ep = sourcePawn && enPassantSquare p == Just (plyTarget ply)
    sourcePawn = sp `elem` [Just (White, Pawn), Just (Black, Pawn)]
    sp = pieceAt p $ plySource ply
    newPosition = unsafeDoPly p ply

checkmateFactor :: Position -> Int
checkmateFactor position = result where
    result = if (null $ legalPlies position)
        then 1000*side
        else 0
    side = if color position == White
        then -1
        else 1

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
