{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.AppModel
    ( Piece
    , ResponseMethod(..)
    , AppModel(..)
    , boardState
    , nextPly
    , chessPosition
    , previousPositions
    , showPromotionMenu
    , autoQueen
    , boardRotated
    , randomGenerator
    , autoRespond
    , responseMethod
    , initModel
    , isWhiteTurn
    , getBoardState
    , getPathOrColor
    , validateMove
    , calculateMove
    , getPromotedPly
    , getPly
    ) where

import Control.Lens
import Data.Maybe
import Game.Chess
import Data.Text (Text)
import System.Random
import qualified Monomer as M

import Model.AI

type Piece = (Color, PieceType)

data ResponseMethod
    = RandomResponse
    | MinimaxResponse
    deriving (Eq, Show)

data AppModel = AppModel
    { _amBoardState :: [[Piece]]
    , _amChessPosition :: Position
    , _amPreviousPositions :: [Position]
    , _amNextPly :: Maybe Ply
    , _amShowPromotionMenu :: Bool
    , _amAutoQueen :: Bool
    , _amBoardRotated :: Bool
    , _amRandomGenerator :: StdGen
    , _amAutoRespond :: Bool
    , _amResponseMethod :: ResponseMethod
    } deriving (Eq, Show)

makeLensesWith abbreviatedFields 'AppModel

initModel :: StdGen -> AppModel
initModel g = AppModel
    { _amBoardState = getBoardState False startpos
    , _amChessPosition = startpos
    , _amPreviousPositions = []
    , _amNextPly = Nothing
    , _amShowPromotionMenu = False
    , _amAutoQueen = False
    , _amBoardRotated = False
    , _amRandomGenerator = g
    , _amAutoRespond = False
    , _amResponseMethod = RandomResponse
    }

isWhiteTurn :: AppModel -> Bool
isWhiteTurn model = color (model ^. chessPosition) == White

getBoardState :: Bool -> Position -> [[Piece]]
getBoardState r position = setPiece . getSquare r <$> [0..63] where
    setPiece square = let p = pieceAt position square in if null p
        then []
        else [fromJust p]

getPathOrColor :: AppModel -> Piece -> Either Text M.Color
getPathOrColor model (color, pieceType) = Left imagePath where
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
        King -> if inCheck color (model ^. chessPosition)
            then "KC"
            else "K"

validateMove :: AppModel -> ([[Piece]], Int, Int) -> Bool
validateMove model info = valid where
    valid = getPromotedPly model info Queen `elem` legal
    legal = legalPlies $ model ^. chessPosition

calculateMove :: AppModel -> (Maybe Ply, StdGen)
calculateMove model = (ply, g) where
    (ply, g) = case model ^. responseMethod of
        RandomResponse -> randomMove position rand
        MinimaxResponse -> (minimaxMove position 2, rand)
    position = model ^. chessPosition
    rand = model ^. randomGenerator

getPromotedPly
    :: AppModel
    -> ([[Piece]], Int, Int)
    -> PieceType
    -> Ply
getPromotedPly model info = promotePly model ply where
    ply = getPly (model ^. boardRotated) info

getPly :: Bool -> ([[Piece]], Int, Int) -> Ply
getPly r (_, ixTo, ixFrom) = move (f ixFrom) (f ixTo) where
    f = getSquare r

promotePly :: AppModel -> Ply -> PieceType -> Ply
promotePly model ply pieceType = newPly where
    newPly = if promotedPly `elem` legal
        then promotedPly
        else ply
    promotedPly = ply `promoteTo` pieceType
    legal = legalPlies $ model ^. chessPosition

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
    rotatedSquares =
        [ H1, G1, F1, E1, D1, C1, B1, A1
        , H2, G2, F2, E2, D2, C2, B2, A2
        , H3, G3, F3, E3, D3, C3, B3, A3
        , H4, G4, F4, E4, D4, C4, B4, A4
        , H5, G5, F5, E5, D5, C5, B5, A5
        , H6, G6, F6, E6, D6, C6, B6, A6
        , H7, G7, F7, E7, D7, C7, B7, A7
        , H8, G8, F8, E8, D8, C8, B8, A8
        ]
