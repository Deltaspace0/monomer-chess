{-# LANGUAGE RecordWildCards #-}

module Composites.TablebasePanel
    ( tablebasePanel
    ) where

import Control.Lens
import Data.List (intersperse)
import Data.Maybe
import Game.Chess
import Monomer

import Model.Tablebase

data TablebaseEvent = EventDoPly (Maybe Ply) deriving (Eq, Show)

tablebasePanel
    :: (CompositeModel sp, CompositeEvent ep)
    => ALens' sp TablebaseData
    -> (Maybe Ply -> ep)
    -> WidgetNode sp ep
tablebasePanel field eventDoPly = node where
    node = composite "tablebasePanel" field buildUI eventHandler
    eventHandler = handleEvent eventDoPly

handleEvent
    :: (Maybe Ply -> ep)
    -> EventHandler TablebaseData TablebaseEvent sp ep
handleEvent eventDoPly _ _ _ (EventDoPly ply) = response where
    response = [Report $ eventDoPly ply]

buildUI :: UIBuilder TablebaseData TablebaseEvent
buildUI _ TablebaseData{..} = tree where
    tree = vstack16
        [ vscroll (label_ (fromMaybe initMsg _tbdStatusMessage) [multiline])
            `styleBasic` [sizeReqH $ fixedSize 64]
        , hgrid_ [childSpacing_ 16]
            [ label "Move"
            , label "DTZ/DTM"
            ]
        , separatorLine
        , vscroll $ vstack16 $ intersperse separatorLine moveStacks
        ]
    vstack4 = vstack_ [childSpacing_ 4]
    vstack16 = vstack_ [childSpacing_ 16]
    initMsg = "Please make a move or refresh analysis"
    moveStacks = map snd $ filter (not . null . fst) $ map makePair
        [ (_tbdWinMoves, "Winning moves:")
        , (_tbdDrawMoves, "Drawing moves:")
        , (_tbdLoseMoves, "Losing moves:")
        ]
    makePair (x, caption) = (x, vstack4 $ (label caption):(makeLine <$> x))
    makeLine (sanMove, info, ply) = hgrid_ [childSpacing_ 16]
        [ button sanMove $ EventDoPly ply
        , label info
        ]
