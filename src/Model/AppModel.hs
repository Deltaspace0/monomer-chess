{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.AppModel
    ( module Model.Piece
    , AppModel(..)
    , boardState
    , initBoardState
    , allPawns
    , initModel
    , getPathOrColor
    , validateMove
    ) where

import Control.Lens
import Data.Text (Text)
import Monomer

import Model.Piece

data AppModel = AppModel
    { _amBoardState :: [[Piece]]
    , _amInitBoardState :: [[Piece]]
    , _amAllPawns :: Bool
    } deriving (Eq, Show)

makeLensesWith abbreviatedFields 'AppModel

initModel :: AppModel
initModel = AppModel initBoard initBoard False where
    initBoard =
        [ [BR], [BN], [BB], [BQ], [BK], [BB], [BN], [BR]
        , [BP], [BP], [BP], [BP], [BP], [BP], [BP], [BP]
        , [], [], [], [], [], [], [], []
        , [], [], [], [], [], [], [], []
        , [], [], [], [], [], [], [], []
        , [], [], [], [], [], [], [], []
        , [WP], [WP], [WP], [WP], [WP], [WP], [WP], [WP]
        , [WR], [WN], [WB], [WQ], [WK], [WB], [WN], [WR]
        ]

getPathOrColor :: AppModel -> Piece -> Either Text Color
getPathOrColor model piece = if model ^. allPawns
    then case piece of
        BR -> Left "assets/chess-pieces/bP.png"
        BN -> Left "assets/chess-pieces/bP.png"
        BB -> Left "assets/chess-pieces/bP.png"
        BQ -> Left "assets/chess-pieces/bP.png"
        BK -> Left "assets/chess-pieces/bP.png"
        BP -> Left "assets/chess-pieces/bP.png"
        WR -> Left "assets/chess-pieces/wP.png"
        WN -> Left "assets/chess-pieces/wP.png"
        WB -> Left "assets/chess-pieces/wP.png"
        WQ -> Left "assets/chess-pieces/wP.png"
        WK -> Left "assets/chess-pieces/wP.png"
        WP -> Left "assets/chess-pieces/wP.png"
    else case piece of
        BR -> Left "assets/chess-pieces/bR.png"
        BN -> Left "assets/chess-pieces/bN.png"
        BB -> Left "assets/chess-pieces/bB.png"
        BQ -> Left "assets/chess-pieces/bQ.png"
        BK -> Left "assets/chess-pieces/bK.png"
        BP -> Left "assets/chess-pieces/bP.png"
        WR -> Left "assets/chess-pieces/wR.png"
        WN -> Left "assets/chess-pieces/wN.png"
        WB -> Left "assets/chess-pieces/wB.png"
        WQ -> Left "assets/chess-pieces/wQ.png"
        WK -> Left "assets/chess-pieces/wK.png"
        WP -> Left "assets/chess-pieces/wP.png"

validateMove :: ([[Piece]], Int, Int) -> Bool
validateMove (board, ixTo, _) = emptyDestination || notKing where
    emptyDestination = null $ board!!ixTo
    notKing = not $ head (board!!ixTo) `elem` [WK, BK]
