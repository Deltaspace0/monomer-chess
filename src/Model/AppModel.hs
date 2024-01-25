{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.AppModel
    ( module Model.AI
    , module Model.FENData
    , AppModel(..)
    , boardState
    , boardStateReversed
    , nextPly
    , chessPosition
    , previousPositions
    , showTwoBoards
    , showEditMenu
    , showPromotionMenu
    , errorMessage
    , autoQueen
    , boardRotated
    , autoRespond
    , calculatingResponse
    , sanMoves
    , forsythEdwards
    , fenData
    , aiData
    , initModel
    , isWhiteTurn
    , getPathOrColor
    , validateMove
    , getPromotedPly
    , getPly
    , chessPieces
    ) where

import Control.Lens
import Game.Chess
import Data.Text (pack, Text)
import qualified Monomer as M

import Model.AI
import Model.FENData

data AppModel = AppModel
    { _amBoardState :: [[Piece]]
    , _amBoardStateReversed :: [[Piece]]
    , _amChessPosition :: Position
    , _amPreviousPositions :: [(Position, Text)]
    , _amNextPly :: Maybe Ply
    , _amShowTwoBoards :: Bool
    , _amShowEditMenu :: Bool
    , _amShowPromotionMenu :: Bool
    , _amErrorMessage :: Maybe Text
    , _amAutoQueen :: Bool
    , _amBoardRotated :: Bool
    , _amAutoRespond :: Bool
    , _amCalculatingResponse :: Bool
    , _amSanMoves :: Text
    , _amForsythEdwards :: Text
    , _amFenData :: FENData
    , _amAiData :: AIData
    } deriving (Eq, Show)

makeLensesWith abbreviatedFields 'AppModel

initModel :: AppModel
initModel = AppModel
    { _amBoardState = getBoardState False startpos
    , _amBoardStateReversed = getBoardState True startpos
    , _amChessPosition = startpos
    , _amPreviousPositions = []
    , _amNextPly = Nothing
    , _amShowTwoBoards = False
    , _amShowEditMenu = False
    , _amShowPromotionMenu = False
    , _amErrorMessage = Nothing
    , _amAutoQueen = False
    , _amBoardRotated = False
    , _amAutoRespond = True
    , _amCalculatingResponse = False
    , _amSanMoves = ""
    , _amForsythEdwards = pack $ toFEN startpos
    , _amFenData = getFenData startpos
    , _amAiData = initAI
    }

isWhiteTurn :: AppModel -> Bool
isWhiteTurn AppModel{..} = color _amChessPosition == White

getPathOrColor :: AppModel -> Piece -> Either Text M.Color
getPathOrColor AppModel{..} (color, pieceType) = result where
    result = Left $ "assets/chess-pieces/" <> c <> p <> ".png"
    c = case color of
        White -> "w"
        Black -> "b"
    p = case pieceType of
        Pawn -> "P"
        Knight -> "N"
        Bishop -> "B"
        Rook -> "R"
        Queen -> "Q"
        King -> if check && not _amShowEditMenu
            then "KC"
            else "K"
    check = inCheck color _amChessPosition

validateMove :: AppModel -> Bool -> ([[Piece]], Int, Int) -> Bool
validateMove model@(AppModel{..}) r info = valid where
    valid = getPromotedPly model r info Queen `elem` legal
    legal = legalPlies _amChessPosition

getPromotedPly
    :: AppModel
    -> Bool
    -> ([[Piece]], Int, Int)
    -> PieceType
    -> Ply
getPromotedPly model r info = promotePly model $ getPly r info

getPly :: Bool -> ([[Piece]], Int, Int) -> Ply
getPly r (_, ixTo, ixFrom) = move (f ixFrom) (f ixTo) where
    f = getSquare r

promotePly :: AppModel -> Ply -> PieceType -> Ply
promotePly AppModel{..} ply pieceType = newPly where
    newPly = if promotedPly `elem` legal
        then promotedPly
        else ply
    promotedPly = ply `promoteTo` pieceType
    legal = legalPlies _amChessPosition

chessPieces :: [[Piece]]
chessPieces =
    [ [(White, Pawn)]
    , [(White, Knight)]
    , [(White, Bishop)]
    , [(White, Rook)]
    , [(White, Queen)]
    , [(White, King)]
    , [(Black, Pawn)]
    , [(Black, Knight)]
    , [(Black, Bishop)]
    , [(Black, Rook)]
    , [(Black, Queen)]
    , [(Black, King)]
    ]
