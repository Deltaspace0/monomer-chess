{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.AppModel
    ( module Model.FENData
    , ResponseMethod(..)
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
    , randomGenerator
    , autoRespond
    , responseMethod
    , mctsRuns
    , minimaxDepth
    , minimaxEvaluation
    , calculatingResponse
    , sanMoves
    , forsythEdwards
    , fenData
    , initModel
    , isWhiteTurn
    , getPathOrColor
    , validateMove
    , calculateMove
    , getPromotedPly
    , getPly
    , chessPieces
    ) where

import Control.Lens
import Game.Chess
import Data.Text (pack, Text)
import System.Random
import qualified Monomer as M

import Model.AI
import Model.FENData

data ResponseMethod
    = RandomResponse
    | MinimaxResponse
    | MCTSResponse
    deriving (Eq, Show)

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
    , _amRandomGenerator :: StdGen
    , _amAutoRespond :: Bool
    , _amResponseMethod :: ResponseMethod
    , _amMctsRuns :: Int
    , _amMinimaxDepth :: Int
    , _amMinimaxEvaluation :: Maybe Int
    , _amCalculatingResponse :: Bool
    , _amSanMoves :: Text
    , _amForsythEdwards :: Text
    , _amFenData :: FENData
    } deriving (Eq, Show)

makeLensesWith abbreviatedFields 'AppModel

initModel :: StdGen -> AppModel
initModel g = AppModel
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
    , _amRandomGenerator = g
    , _amAutoRespond = True
    , _amResponseMethod = MCTSResponse
    , _amMctsRuns = 2000
    , _amMinimaxDepth = 4
    , _amMinimaxEvaluation = Nothing
    , _amCalculatingResponse = False
    , _amSanMoves = ""
    , _amForsythEdwards = pack $ toFEN startpos
    , _amFenData = getFenData startpos
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

calculateMove :: AppModel -> (Maybe Ply, StdGen, Maybe Int)
calculateMove AppModel{..} = result where
    result = case _amResponseMethod of
        RandomResponse -> (randomPly, nextRand, Nothing)
        MinimaxResponse -> (mmPly, rand, Just eval)
        MCTSResponse -> (mctsPly, mctsRand, Nothing)
    (randomPly, nextRand) = randomMove _amChessPosition rand
    (mmPly, eval) = minimaxMove _amChessPosition _amMinimaxDepth
    (mctsPly, mctsRand) = mctsMove _amChessPosition rand _amMctsRuns
    rand = _amRandomGenerator

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
