{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.AppModel
    ( module Model.AI
    , module Model.FENData
    , module Model.Tablebase
    , module Model.UCI
    , AppModel(..)
    , boardState
    , boardStateReversed
    , nextPly
    , chessPosition
    , positionTree
    , positionTreePath
    , currentPlyNumber
    , showTwoBoards
    , showEditMenu
    , showPromotionMenu
    , showTablebase
    , tablebaseData
    , errorMessage
    , autoQueen
    , boardRotated
    , autoRespond
    , responseThread
    , sanMoves
    , forsythEdwards
    , fenData
    , aiData
    , uciData
    , uciLogs
    , uciRecordLogs
    , initModel
    , isWhiteTurn
    , indexPositionTree
    , indexTree
    , insertTree
    , pruneTree
    , getTreeTailDepth
    , treeToSanMoves
    , getPathOrColor
    , validateMove
    , getPromotedPly
    , chessPieces
    ) where

import Control.Concurrent
import Control.Lens
import Game.Chess
import Game.Chess.PGN
import Data.Text (pack, Text)
import Data.Maybe
import Data.Tree (Tree(..))
import qualified Monomer as M

import Model.AI
import Model.FENData
import Model.Tablebase
import Model.UCI

type PP = (Position, Maybe Ply, String, Text)

data AppModel = AppModel
    { _amBoardState :: [[Piece]]
    , _amBoardStateReversed :: [[Piece]]
    , _amChessPosition :: Position
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
    , _amAutoQueen :: Bool
    , _amBoardRotated :: Bool
    , _amAutoRespond :: Bool
    , _amResponseThread :: Maybe ThreadId
    , _amSanMoves :: Text
    , _amForsythEdwards :: Text
    , _amFenData :: FENData
    , _amAiData :: AIData
    , _amUciData :: UCIData
    , _amUciLogs :: Text
    , _amUciRecordLogs :: Bool
    } deriving (Eq, Show)

makeLensesWith abbreviatedFields 'AppModel

initModel :: AppModel
initModel = AppModel
    { _amBoardState = startState
    , _amBoardStateReversed = startStateReversed
    , _amChessPosition = startpos
    , _amPositionTree = Node (startpos, Nothing, "", "") []
    , _amPositionTreePath = []
    , _amCurrentPlyNumber = 0
    , _amNextPly = Nothing
    , _amShowTwoBoards = False
    , _amShowEditMenu = False
    , _amShowPromotionMenu = False
    , _amShowTablebase = False
    , _amTablebaseData = defaultTablebaseData
    , _amErrorMessage = Nothing
    , _amAutoQueen = False
    , _amBoardRotated = False
    , _amAutoRespond = True
    , _amResponseThread = Nothing
    , _amSanMoves = ""
    , _amForsythEdwards = pack $ toFEN startpos
    , _amFenData = getFenData startpos
    , _amAiData = initAI
    , _amUciData = defaultUciData
    , _amUciLogs = ""
    , _amUciRecordLogs = False
    } where
        (startState, startStateReversed) = getBoardStates startpos

isWhiteTurn :: AppModel -> Bool
isWhiteTurn AppModel{..} = color _amChessPosition == White

indexPositionTree :: AppModel -> Int -> PP
indexPositionTree AppModel{..} i = result where
    Node result _ = indexTree (take i _amPositionTreePath) _amPositionTree

indexTree :: [Int] -> Tree a -> Tree a
indexTree [] tree = tree
indexTree (x:xs) (Node _ childNodes) = indexTree xs $ childNodes!!x

insertTree :: [Int] -> PP -> Tree PP -> Either Int (Tree PP)
insertTree [] pp@(_, _, _, t) (Node v childNodes) = result where
    result = if existingIndex == -1
        then Right $ Node v $ (Node pp []):childNodes
        else Left existingIndex
    existingIndex = findExistingIndex childNodes 0
    findExistingIndex [] _ = -1
    findExistingIndex ((Node (_, _, _, t') _):xs) i = if t == t'
        then i
        else findExistingIndex xs $ i+1
insertTree (x:xs) pp (Node v childNodes) = result where
    result = case insertTree xs pp (childNodes!!x) of
        Left existingIndex -> Left existingIndex
        Right newTree -> Right $ Node v $ childNodes & ix x .~ newTree

pruneTree :: [Int] -> Tree a -> Tree a
pruneTree [] _ = error "empty prune path"
pruneTree (x:xs) (Node v childNodes) = Node v $ if null xs
    then take x childNodes <> drop (x+1) childNodes
    else childNodes & ix x %~ pruneTree xs

getTreeTailDepth :: [Int] -> Tree a -> Int
getTreeTailDepth path tree = go $ indexTree path tree where
    go (Node _ []) = 0
    go (Node _ childNodes) = 1 + go (head childNodes)

treeToSanMoves :: Tree PP -> Text
treeToSanMoves (Node _ childNodes) = result where
    result = pack $ show $ gameDoc depthFirst game
    game = gameFromForest [] tree Undecided
    tree = (extractPly <$>) <$> childNodes
    extractPly (_, ply, _, _) = fromJust ply

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
