{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.AI.MCTS
    ( mctsMove
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

mctsMove :: Position -> StdGen -> (Maybe Ply, StdGen)
mctsMove position randomGenerator = (ply, g) where
    ply = snd <$> (getBestNode $ finalTree ^. childNodes)
    (finalTree, g) = mctsRepeat tree randomGenerator 1000
    tree = initializeTree position

getBestNode :: [(Tree, Ply)] -> Maybe (Tree, Ply)
getBestNode [] = Nothing
getBestNode (x:xs) = result where
    result = Just $ if null other || simulations > otherSimulations
        then x
        else fromJust other
    simulations = (fst x) ^. statSimulations
    otherSimulations = (fst $ fromJust other) ^. statSimulations
    other = getBestNode xs

mctsRepeat :: Tree -> StdGen -> Int -> (Tree, StdGen)
mctsRepeat tree randomGenerator n = result where
    result = if n <= 0
        then (tree, randomGenerator)
        else mctsRepeat nextTree g $ n-1
    (nextTree, g) = monteCarloTreeSearch tree randomGenerator

monteCarloTreeSearch :: Tree -> StdGen -> (Tree, StdGen)
monteCarloTreeSearch tree randomGenerator = r where
    r = if tree ^. statSimulations == 0 || null (tree ^. childNodes)
        then (rolloutTree, gr)
        else (nextTree, g)
    rolloutTree = tree
        & statWins +~ rollout
        & statSimulations +~ 1
    (rollout, gr) = doRollout (tree ^. rootPosition) randomGenerator
    nextTree = tree
        & statWins +~ 1-(subTree' ^. statWins)+(subTree ^. statWins)
        & statSimulations +~ 1
        & childNodes . element i . _1 .~ subTree'
    (subTree', g) = monteCarloTreeSearch subTree randomGenerator
    (subTree, i) = selectChild tree

selectChild :: Tree -> (Tree, Int)
selectChild tree = f $ zipWith (,) subTrees [0..] where
    f [] = error "No child nodes to select"
    f elems@(x@(Tree _ w s _, _):xs)
        | length elems == 1 || s == 0 = x
        | eval w s > eval w' s' = x
        | otherwise = x'
        where
            x'@(Tree _ w' s' _, _) = f xs
    eval w s = let s' = fromIntegral s in w/s' + sqrt (n/s')
    n = 2 * (log $ fromIntegral $ tree ^. statSimulations)
    subTrees = fst <$> tree ^. childNodes

doRollout :: Position -> StdGen -> (Double, StdGen)
doRollout position randomGenerator = result where
    result
        | null legal && inCheck White position = withRand 1
        | null legal && inCheck Black position = withRand 1
        | null legal || insufficientMaterial position = withRand 0.5
        | halfMoveClock position >= 100 = withRand 0.5
        | otherwise = (1-rollout, gr)
    (rollout, gr) = doRollout nextPosition g
    legal = legalPlies position
    nextPosition = unsafeDoPly position $ legal!!i
    (i, g) = randomR (0, length legal-1) randomGenerator
    withRand x = (x, randomGenerator)

initializeTree :: Position -> Tree
initializeTree position = Tree
    { _tRootPosition = position
    , _tStatWins = 0
    , _tStatSimulations = 0
    , _tChildNodes = (\x -> (subTree x, x)) <$> legalPlies position
    } where
        subTree = initializeTree . unsafeDoPly position
