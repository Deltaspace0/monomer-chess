{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.FENData
    ( Piece
    , FENData(..)
    , fenBoardState
    , fenTurn
    , fenCastleWK
    , fenCastleWQ
    , fenCastleBK
    , fenCastleBQ
    , getFenData
    , getFenString
    , getBoardState
    , getSquare
    ) where

import Control.Lens
import Data.List (intersperse)
import Data.List.Split
import Data.Maybe
import Game.Chess

type Piece = (Color, PieceType)

data FENData = FENData
    { _fdFenBoardState :: [[Piece]]
    , _fdFenTurn :: Color
    , _fdFenCastleWK :: Bool
    , _fdFenCastleWQ :: Bool
    , _fdFenCastleBK :: Bool
    , _fdFenCastleBQ :: Bool
    } deriving (Eq, Show)

makeLensesWith abbreviatedFields 'FENData

getFenData :: Bool -> Position -> FENData
getFenData r position = FENData
    { _fdFenBoardState = getBoardState r position
    , _fdFenTurn = color position
    , _fdFenCastleWK = (White, Kingside) `elem` cs
    , _fdFenCastleWQ = (White, Queenside) `elem` cs
    , _fdFenCastleBK = (Black, Kingside) `elem` cs
    , _fdFenCastleBQ = (Black, Queenside) `elem` cs
    } where
        cs = castlingRights position

getFenString :: Bool -> FENData -> String
getFenString r FENData{..} = result where
    result = unwords [boardFen, turnFen, castleFen, "- 0 1"]
    boardFen = concat $ intersperse "/" $ chunksOf 8 $ if r
        then pieceChar <$> reverse _fdFenBoardState
        else pieceChar <$> _fdFenBoardState
    turnFen = if _fdFenTurn == White then "w" else "b"
    castleFen = if null castles then "-" else castles
    castles = concat
        [ if _fdFenCastleWK then "K" else ""
        , if _fdFenCastleWQ then "Q" else ""
        , if _fdFenCastleBK then "k" else ""
        , if _fdFenCastleBQ then "q" else ""
        ]

pieceChar :: [Piece] -> Char
pieceChar [] = '1'
pieceChar [(White, Pawn)] = 'P'
pieceChar [(White, Knight)] = 'N'
pieceChar [(White, Bishop)] = 'B'
pieceChar [(White, Rook)] = 'R'
pieceChar [(White, Queen)] = 'Q'
pieceChar [(White, King)] = 'K'
pieceChar [(Black, Pawn)] = 'p'
pieceChar [(Black, Knight)] = 'n'
pieceChar [(Black, Bishop)] = 'b'
pieceChar [(Black, Rook)] = 'r'
pieceChar [(Black, Queen)] = 'q'
pieceChar [(Black, King)] = 'k'
pieceChar _ = '?'

getBoardState :: Bool -> Position -> [[Piece]]
getBoardState r position = setPiece . getSquare r <$> [0..63] where
    setPiece square = let p = pieceAt position square in if null p
        then []
        else [fromJust p]

getSquare :: Bool -> Int -> Square
getSquare rotated = f where
    f = if rotated
        then (rotatedSquares!!)
        else (squares!!)
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
    rotatedSquares = reverse squares
