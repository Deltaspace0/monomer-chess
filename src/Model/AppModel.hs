{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.AppModel
    ( module Model.AI
    , module Model.FENData
    , module Model.PositionTree
    , module Model.Tablebase
    , module Model.UCI
    , AppModel(..)
    , boardState
    , boardStateReversed
    , nextPly
    , chessPosition
    , evalGroups
    , positionTree
    , positionTreePath
    , currentPlyNumber
    , showTwoBoards
    , showEditMenu
    , showPromotionMenu
    , showTablebase
    , tablebaseData
    , errorMessage
    , showLegal
    , showCoords
    , autoQueen
    , boardRotated
    , responseThread
    , sanMoves
    , forsythEdwards
    , fenData
    , aiData
    , uciData
    , uciIndex
    , uciLogs
    , uciRecordLogs
    , evalProgress
    , evalProgressMVars
    , initModel
    , isWhiteTurn
    , indexPositionTree
    , getPathOrColor
    , validateMove
    , getPromotedPly
    , chessPieces
    ) where

import Control.Concurrent
import Control.Lens
import Data.Text (pack, Text)
import Data.Tree (Tree(..))
import Game.Chess
import qualified Monomer as M

import Model.AI
import Model.FENData
import Model.PositionTree
import Model.Tablebase
import Model.UCI

data AppModel = AppModel
    { _amBoardState :: [[Piece]]
    , _amBoardStateReversed :: [[Piece]]
    , _amChessPosition :: Position
    , _amEvalGroups :: [[(Double, Double)]]
    , _amPositionTree :: Tree PP
    , _amPositionTreePath :: [Int]
    , _amCurrentPlyNumber :: Int
    , _amNextPly :: Maybe Ply
    , _amShowTwoBoards :: Bool
    , _amShowEditMenu :: Bool
    , _amShowPromotionMenu :: Bool
    , _amShowTablebase :: Bool
    , _amTablebaseData :: TablebaseData
    , _amErrorMessage :: Maybe Text
    , _amShowLegal :: Bool
    , _amShowCoords :: Bool
    , _amAutoQueen :: Bool
    , _amBoardRotated :: Bool
    , _amResponseThread :: Maybe ThreadId
    , _amSanMoves :: Text
    , _amForsythEdwards :: Text
    , _amFenData :: FENData
    , _amAiData :: AIData
    , _amUciData :: [UCIData]
    , _amUciIndex :: Int
    , _amUciLogs :: Text
    , _amUciRecordLogs :: Bool
    , _amEvalProgress :: Maybe Text
    , _amEvalProgressMVars :: Maybe (MVar [Int], MVar (Tree PP), MVar ())
    } deriving (Eq, Show)

makeLensesWith abbreviatedFields 'AppModel

initModel :: AppModel
initModel = AppModel
    { _amBoardState = startState
    , _amBoardStateReversed = startStateReversed
    , _amChessPosition = startpos
    , _amEvalGroups = []
    , _amPositionTree = treeFromPosition startpos
    , _amPositionTreePath = []
    , _amCurrentPlyNumber = 0
    , _amNextPly = Nothing
    , _amShowTwoBoards = False
    , _amShowEditMenu = False
    , _amShowPromotionMenu = False
    , _amShowTablebase = False
    , _amTablebaseData = defaultTablebaseData
    , _amErrorMessage = Nothing
    , _amShowLegal = True
    , _amShowCoords = True
    , _amAutoQueen = False
    , _amBoardRotated = False
    , _amResponseThread = Nothing
    , _amSanMoves = ""
    , _amForsythEdwards = pack $ toFEN startpos
    , _amFenData = getFenData startpos
    , _amAiData = initAI
    , _amUciData = [defaultUciData]
    , _amUciIndex = 0
    , _amUciLogs = ""
    , _amUciRecordLogs = False
    , _amEvalProgress = Nothing
    , _amEvalProgressMVars = Nothing
    } where
        (startState, startStateReversed) = getBoardStates startpos

isWhiteTurn :: AppModel -> Bool
isWhiteTurn AppModel{..} = color _amChessPosition == White

indexPositionTree :: AppModel -> Int -> PP
indexPositionTree AppModel{..} i = result where
    Node result _ = indexTree (take i _amPositionTreePath) _amPositionTree

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

validateMove :: AppModel -> ([[Piece]], Int, Int) -> Bool
validateMove model@(AppModel{..}) info@(_, ixTo, ixFrom) = valid where
    valid = abs (ixTo-ixFrom) < 100 && promotedPly `elem` legal
    promotedPly = getPromotedPly model info Queen
    legal = legalPlies _amChessPosition

getPromotedPly :: AppModel -> ([[Piece]], Int, Int) -> PieceType -> Ply
getPromotedPly model (_, ixTo, ixFrom) = promotePly model $ if ixTo >= 500
    then move (squares!!(563-ixFrom)) (squares!!(563-ixTo))
    else move (squares!!ixFrom) (squares!!ixTo)

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
