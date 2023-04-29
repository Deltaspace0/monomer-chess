module Model.AI.PositionEval
    ( PositionEval(..)
    , makePositionEval
    , doPlyEval
    ) where

import Game.Chess

data PositionEval = PositionEval Position Int

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
