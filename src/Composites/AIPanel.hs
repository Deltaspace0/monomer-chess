{-# LANGUAGE RecordWildCards #-}

module Composites.AIPanel
    ( aiPanel
    ) where

import Control.Lens
import Data.Maybe
import Monomer
import TextShow

import Model.AI

aiPanel
    :: (CompositeModel sp, CompositeEvent ep)
    => ALens' sp AIData
    -> WidgetNode sp ep
aiPanel field = node where
    node = composite "aiPanel" field buildUI eventHandler
    eventHandler _ _ _ _ = []

buildUI :: UIBuilder AIData ()
buildUI _ AIData{..} = tree where
    tree = vstack_ [childSpacing_ 16]
        [ hstack_ [childSpacing_ 16]
            [ label "How to calculate:"
            , textDropdownS responseMethod
                [ RandomResponse
                , UCIResponse
                , MinimaxResponse
                , MCTSResponse
                ]
            ]
        , widgetIf (_adResponseMethod == MinimaxResponse) $
            hgrid_ [childSpacing_ 16]
                [ label $ "Minimax depth: " <> (showt _adMinimaxDepth)
                , hslider_ minimaxDepth 1 20 [dragRate 1]
                ]
        , widgetIf (_adResponseMethod == MCTSResponse) $
            hgrid_ [childSpacing_ 16]
                [ label $ "MCTS runs: " <> (showt _adMctsRuns)
                , hslider_ mctsRuns 100 30000 [dragRate 1]
                ]
        , widgetIf (not $ null _adPositionEvaluation) $
            label $ "Evaluation: " <> fromMaybe "" _adPositionEvaluation
        ]
