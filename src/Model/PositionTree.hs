{-# LANGUAGE RecordWildCards #-}

module Model.PositionTree
    ( PP(..)
    , treeFromPosition
    , treeSwitchValue
    , indexTree
    , insertTree
    , pruneTree
    , getTreeTailDepth
    , treeToList
    , treeToSanMoves
    , toPositionTree
    , getNextPP
    ) where

import Control.Lens
import Data.Maybe
import Data.Text (pack, Text)
import Data.Tree (Tree(..))
import Game.Chess
import Game.Chess.PGN
import Game.Chess.SAN

data PP = PP
    { _ppPosition :: Position
    , _ppPly :: Maybe Ply
    , _ppUciMoves :: String
    , _ppSan :: Text
    , _ppEval :: Maybe Double
    } deriving (Eq, Show)

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
