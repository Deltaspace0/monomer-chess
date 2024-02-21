{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.AppModel
    ( module Model.AI
    , module Model.FENData
    , module Model.Tablebase
    , module Model.UCI
    , PP(..)
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
    , treeFromPosition
    , treeSwitchValue
    , indexPositionTree
    , indexTree
    , insertTree
    , pruneTree
    , getTreeTailDepth
    , treeToList
    , treeToSanMoves
    , toPositionTree
    , getNextPP
    , getPathOrColor
    , validateMove
    , getPromotedPly
    , chessPieces
    ) where

import Control.Concurrent
import Control.Lens
import Game.Chess
import Game.Chess.PGN
import Game.Chess.SAN
import Data.Text (pack, Text)
import Data.Maybe
import Data.Tree (Tree(..))
import qualified Monomer as M

import Model.AI
import Model.FENData
import Model.Tablebase
import Model.UCI

data PP = PP
    { _ppPosition :: Position
    , _ppPly :: Maybe Ply
    , _ppUciMoves :: String
    , _ppSan :: Text
    , _ppEval :: Maybe Double
    } deriving (Eq, Show)

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

treeFromPosition :: Position -> Tree PP
treeFromPosition position = Node pp [] where
    pp = PP
        { _ppPosition = position
        , _ppPly = Nothing
        , _ppUciMoves = ""
        , _ppSan = ""
        , _ppEval = Nothing
        }

treeSwitchValue :: a -> Tree a -> Tree a
treeSwitchValue newValue (Node _ childNodes) = Node newValue childNodes

indexPositionTree :: AppModel -> Int -> PP
indexPositionTree AppModel{..} i = result where
    Node result _ = indexTree (take i _amPositionTreePath) _amPositionTree

indexTree :: [Int] -> Tree a -> Tree a
indexTree [] tree = tree
indexTree (x:xs) (Node _ childNodes) = indexTree xs $ childNodes!!x

insertTree :: [Int] -> PP -> Tree PP -> (Tree PP, Maybe Int)
insertTree [] pp (Node v childNodes) = (resultTree, existingIndex) where
    resultTree = Node v $ case existingIndex of
        Nothing -> (Node pp []):childNodes
        Just exi -> childNodes & ix exi %~ treeSwitchValue pp
    existingIndex = findExistingIndex childNodes 0
    findExistingIndex [] _ = Nothing
    findExistingIndex ((Node pp' _):xs) i = if _ppSan pp == _ppSan pp'
        then Just i
        else findExistingIndex xs $ i+1
insertTree (x:xs) pp (Node v childNodes) = (resultTree, existingIndex) where
    resultTree = Node v $ childNodes & ix x .~ newTree
    (newTree, existingIndex) = insertTree xs pp $ childNodes!!x

pruneTree :: [Int] -> Tree a -> Tree a
pruneTree [] _ = error "empty prune path"
pruneTree (x:xs) (Node v childNodes) = Node v $ if null xs
    then take x childNodes <> drop (x+1) childNodes
    else childNodes & ix x %~ pruneTree xs

getTreeTailDepth :: [Int] -> Tree a -> Int
getTreeTailDepth path tree = go $ indexTree path tree where
    go (Node _ []) = 0
    go (Node _ childNodes) = 1 + go (head childNodes)

treeToList :: [Int] -> Tree a -> [a]
treeToList [] (Node v _) = [v]
treeToList (x:xs) (Node v childNodes) = v:(treeToList xs $ childNodes!!x)

treeToSanMoves :: Tree PP -> Text
treeToSanMoves (Node pp childNodes) = result where
    result = pack $ show $ gameDoc depthFirst game
    game = gameFromForest tags tree Undecided
    tags = [("FEN", pack $ toFEN $ _ppPosition pp)]
    tree = (fromJust . _ppPly <$>) <$> childNodes

toPositionTree :: Position -> [Tree Ply] -> Tree PP
toPositionTree position treePlies = buildTree initPP treePlies where
    buildTree pp trees = Node pp $ f <$> trees where
        f (Node ply childPlies) = buildTree (getNextPP pp ply) childPlies
    Node initPP _ = treeFromPosition position

getNextPP :: PP -> Ply -> PP
getNextPP PP{..} ply = PP
    { _ppPosition = unsafeDoPly _ppPosition ply
    , _ppPly = Just ply
    , _ppUciMoves = _ppUciMoves <> " " <> toUCI ply
    , _ppSan = pack $ unsafeToSAN _ppPosition ply
    , _ppEval = Nothing
    }

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
