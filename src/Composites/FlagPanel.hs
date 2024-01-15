{-# LANGUAGE FlexibleContexts #-}

module Composites.FlagPanel
    ( flagPanel
    ) where

import Control.Lens
import Game.Chess
import Monomer hiding (Color)

import Model.FENData

data FlagEvent = EventUpdateFEN deriving (Eq, Show)

flagPanel
    :: (CompositeModel sp, CompositeEvent ep)
    => ALens' sp FENData
    -> ep
    -> WidgetNode sp ep
flagPanel field event = node where
    node = composite "flagPanel" field buildUI eventHandler
    eventHandler = handleEvent event

handleEvent :: ep -> EventHandler FENData FlagEvent sp ep
handleEvent event _ _ _ EventUpdateFEN = [Report event]

buildUI :: UIBuilder FENData FlagEvent
buildUI _ _ = tree where
    tree = hstack_ [childSpacing_ 16]
        [ vstack'
            [ labeledRadio' "White's turn" White fenTurn
            , labeledRadio' "Black's turn" Black fenTurn
            ]
        , separatorLine
        , hgrid_ [childSpacing_ 16]
            [ vstack'
                [ label "White:"
                , labeledCheckbox' "O-O" fenCastleWK
                , labeledCheckbox' "O-O-O" fenCastleWQ
                ]
            , vstack'
                [ label "Black:"
                , labeledCheckbox' "O-O" fenCastleBK
                , labeledCheckbox' "O-O-O" fenCastleBQ
                ]
            ]
        ]
    vstack' = vstack_ [childSpacing_ 16]
    labeledRadio' t v l = labeledRadio_ t v l [onChange updateR]
    labeledCheckbox' t l = labeledCheckbox_ t l [onChange updateC]
    updateR :: Color -> FlagEvent
    updateR = const EventUpdateFEN
    updateC :: Bool -> FlagEvent
    updateC = const EventUpdateFEN
