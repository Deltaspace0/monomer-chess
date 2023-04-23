{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.AppModel
    ( AppModel(..)
    , boardState
    , initBoardState
    , allPawns
    , initModel
    , getPathOrColor
    , validateMove
    ) where

import Control.Lens
import Game.Chess
import Data.Text (Text)
import qualified Monomer as M

type Piece = (Color, PieceType)

data AppModel = AppModel
    { _amBoardState :: [[Piece]]
    , _amInitBoardState :: [[Piece]]
    , _amAllPawns :: Bool
    } deriving (Eq, Show)

makeLensesWith abbreviatedFields 'AppModel

initModel :: AppModel
initModel = AppModel initBoard initBoard False where
    initBoard =
        [ [bR], [bN], [bB], [bQ], [bK], [bB], [bN], [bR]
        , [bP], [bP], [bP], [bP], [bP], [bP], [bP], [bP]
        , [], [], [], [], [], [], [], []
        , [], [], [], [], [], [], [], []
        , [], [], [], [], [], [], [], []
        , [], [], [], [], [], [], [], []
        , [wP], [wP], [wP], [wP], [wP], [wP], [wP], [wP]
        , [wR], [wN], [wB], [wQ], [wK], [wB], [wN], [wR]
        ]
    bR = (Black, Rook)
    bN = (Black, Knight)
    bB = (Black, Bishop)
    bQ = (Black, Queen)
    bK = (Black, King)
    bP = (Black, Pawn)
    wR = (White, Rook)
    wN = (White, Knight)
    wB = (White, Bishop)
    wQ = (White, Queen)
    wK = (White, King)
    wP = (White, Pawn)

getPathOrColor :: AppModel -> Piece -> Either Text M.Color
getPathOrColor model (color, pieceType) = Left imagePath where
    imagePath = if model ^. allPawns
        then buildPath Pawn
        else buildPath pieceType
    buildPath p = "assets/chess-pieces/" <> c <> f p <> ".png"
    c = case color of
        White -> "w"
        Black -> "b"
    f p = case p of
        Pawn -> "P"
        Knight -> "N"
        Bishop -> "B"
        Rook -> "R"
        Queen -> "Q"
        King -> "K"

validateMove :: AppModel -> ([[Piece]], Int, Int) -> Bool
validateMove model (board, ixTo, _) = any id validConditions where
    validConditions =
        [ model ^. allPawns
        , null $ board!!ixTo
        , snd (head (board!!ixTo)) /= King
        ]
