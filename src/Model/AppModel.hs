{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.AppModel
    ( Piece
    , AppModel(..)
    , boardState
    , nextPly
    , chessPosition
    , showPromotionMenu
    , initModel
    , isWhiteTurn
    , getBoardState
    , getPathOrColor
    , validateMove
    , getPly
    , promotePly
    ) where

import Control.Lens
import Data.Maybe
import Game.Chess
import Data.Text (Text)
import qualified Monomer as M

type Piece = (Color, PieceType)

data AppModel = AppModel
    { _amBoardState :: [[Piece]]
    , _amChessPosition :: Position
    , _amNextPly :: Maybe Ply
    , _amShowPromotionMenu :: Bool
    } deriving (Eq, Show)

makeLensesWith abbreviatedFields 'AppModel

initModel :: AppModel
initModel = AppModel initBoardState startpos Nothing False where
    initBoardState = getBoardState startpos

isWhiteTurn :: AppModel -> Bool
isWhiteTurn model = color (model ^. chessPosition) == White

getBoardState :: Position -> [[Piece]]
getBoardState position = setPiece . getSquare <$> [0..63] where
    setPiece square = let p = pieceAt position square in if null p
        then []
        else [fromJust p]

getPathOrColor :: Piece -> Either Text M.Color
getPathOrColor (color, pieceType) = Left imagePath where
    imagePath = "assets/chess-pieces/" <> c <> p <> ".png"
    c = case color of
        White -> "w"
        Black -> "b"
    p = case pieceType of
        Pawn -> "P"
        Knight -> "N"
        Bishop -> "B"
        Rook -> "R"
        Queen -> "Q"
        King -> "K"

validateMove :: AppModel -> ([[Piece]], Int, Int) -> Bool
validateMove model info = valid where
    valid = promotePly model (getPly info) Queen `elem` legal
    legal = legalPlies $ model ^. chessPosition

getPly :: ([[Piece]], Int, Int) -> Ply
getPly (_, ixTo, ixFrom) = move (getSquare ixFrom) (getSquare ixTo)

promotePly :: AppModel -> Ply -> PieceType -> Ply
promotePly model ply pieceType = newPly where
    newPly = if promotedPly `elem` legal
        then promotedPly
        else ply
    promotedPly = ply `promoteTo` pieceType
    legal = legalPlies $ model ^. chessPosition

getSquare :: Int -> Square
getSquare = (squares!!) where
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
