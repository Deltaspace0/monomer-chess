{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.AppModel
    ( module Model.FENData
    , ResponseMethod(..)
    , AppModel(..)
    , boardState
    , nextPly
    , chessPosition
    , previousPositions
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
    , thinkingAnimation
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
    , _amChessPosition :: Position
    , _amPreviousPositions :: [(Position, Text)]
    , _amNextPly :: Maybe Ply
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
    , _amThinkingAnimation :: Text
    , _amSanMoves :: Text
    , _amForsythEdwards :: Text
    , _amFenData :: FENData
    } deriving (Eq, Show)

makeLensesWith abbreviatedFields 'AppModel

initModel :: StdGen -> AppModel
initModel g = AppModel
    { _amBoardState = getBoardState False startpos
    , _amChessPosition = startpos
    , _amPreviousPositions = []
    , _amNextPly = Nothing
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
    , _amThinkingAnimation = ""
    , _amSanMoves = ""
    , _amForsythEdwards = pack $ toFEN startpos
    , _amFenData = getFenData False startpos
    }

isWhiteTurn :: AppModel -> Bool
isWhiteTurn model = color (model ^. chessPosition) == White

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
        King -> if check && not (model ^. showEditMenu)
            then "KC"
            else "K"
    check = inCheck color (model ^. chessPosition)

validateMove :: AppModel -> ([[Piece]], Int, Int) -> Bool
validateMove model info = valid where
    valid = getPromotedPly model info Queen `elem` legal
    legal = legalPlies $ model ^. chessPosition

calculateMove :: AppModel -> (Maybe Ply, StdGen, Maybe Int)
calculateMove model = result where
    result = case model ^. responseMethod of
        RandomResponse -> (randomPly, nextRand, Nothing)
        MinimaxResponse -> (minimaxPly, rand, Just eval)
        MCTSResponse -> (mctsPly, mctsRand, Nothing)
    (randomPly, nextRand) = randomMove position rand
    (minimaxPly, eval) = minimaxMove position depth
    (mctsPly, mctsRand) = mctsMove position rand $ model ^. mctsRuns
    position = model ^. chessPosition
    depth = model ^. minimaxDepth
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
