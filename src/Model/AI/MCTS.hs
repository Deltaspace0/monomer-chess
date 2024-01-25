{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.AI.MCTS
    ( Tree(..)
    , rootPosition
    , statWins
    , statSimulations
    , childNodes
    , mctsMove
    ) where

import Control.Lens
import Data.Maybe
import Game.Chess
import System.Random

data Tree = Tree
    { _tRootPosition :: Position
    , _tStatWins :: Double
    , _tStatSimulations :: Int
    , _tChildNodes :: [(Tree, Ply)]
    }

makeLensesWith abbreviatedFields 'Tree

mctsMove :: Position -> Int -> IO (Maybe Ply)
mctsMove position runs = do
    finalTree <- mctsRepeat runs $ initializeTree position
    return $ snd <$> (getBestNode $ finalTree ^. childNodes)

getBestNode :: [(Tree, Ply)] -> Maybe (Tree, Ply)
getBestNode [] = Nothing
getBestNode (x:xs) = result where
    result = Just $ if null other || simulations > otherSimulations
        then x
        else fromJust other
    simulations = (fst x) ^. statSimulations
    otherSimulations = (fst $ fromJust other) ^. statSimulations
    other = getBestNode xs

mctsRepeat :: Int -> Tree -> IO Tree
mctsRepeat n tree = if n <= 0
    then pure tree
    else monteCarloTreeSearch tree >>= mctsRepeat (n-1)

monteCarloTreeSearch :: Tree -> IO Tree
monteCarloTreeSearch tree@(Tree{..}) = result where
    result = if _tStatSimulations == 0 || null _tChildNodes
        then doRollout _tRootPosition <&> \x -> tree
            & statWins +~ x
            & statSimulations +~ 1
        else monteCarloTreeSearch subTree <&> \x -> tree
            & statWins +~ 1-(x ^. statWins)+(subTree ^. statWins)
            & statSimulations +~ 1
            & childNodes . element i . _1 .~ x
    (subTree, i) = selectChild tree

selectChild :: Tree -> (Tree, Int)
selectChild Tree{..} = f $ zip subTrees [0..] where
    f [] = error "No child nodes to select"
    f elems@(x@(Tree _ w s _, _):xs)
        | length elems == 1 || s == 0 = x
        | eval w s > eval w' s' = x
        | otherwise = x'
        where
            x'@(Tree _ w' s' _, _) = f xs
    eval w s = let s' = fromIntegral s in w/s' + sqrt (n/s')
    n = 2 * (log $ fromIntegral _tStatSimulations)
    subTrees = fst <$> _tChildNodes

doRollout :: Position -> IO Double
doRollout position
    | null legal && inCheck White position = pure 1
    | null legal && inCheck Black position = pure 1
    | null legal || insufficientMaterial position = pure 0.5
    | halfMoveClock position >= 100 = pure 0.5
    | otherwise = (1-) <$> rollout
    where
        legal = legalPlies position
        rollout = i >>= doRollout . unsafeDoPly position . (legal!!)
        i = randomRIO (0, length legal-1)

initializeTree :: Position -> Tree
initializeTree position = Tree
    { _tRootPosition = position
    , _tStatWins = 0
    , _tStatSimulations = 0
    , _tChildNodes = (\x -> (subTree x, x)) <$> legalPlies position
    } where
        subTree = initializeTree . unsafeDoPly position
