{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Composites.UCIOptionsPanel
    ( uciOptionsPanel
    ) where

import Control.Concurrent
import Control.Lens
import Data.Text (pack)
import Monomer
import TextShow

import Model.UCIOptions

data OptionsEvent
    = EventApply Int
    | EventSetActiveOptions [OptionUCI]
    deriving (Eq, Show)

type EventHandle sp ep
    = UCIOptions -> [EventResponse UCIOptions OptionsEvent sp ep]

uciOptionsPanel
    :: (CompositeModel sp, CompositeEvent ep)
    => ALens' sp UCIOptions
    -> MVar String
    -> WidgetNode sp ep
uciOptionsPanel field requestMVar = node where
    node = composite "uciOptionsPanel" field buildUI eventHandler
    eventHandler = handleEvent requestMVar

handleEvent :: MVar String -> EventHandler UCIOptions OptionsEvent sp ep
handleEvent requestMVar _ _ model event = case event of
    EventApply i -> applyHandle i requestMVar model
    EventSetActiveOptions v -> setActiveOptionsHandle v model

applyHandle :: Int -> MVar String -> EventHandle sp ep
applyHandle i requestMVar UCIOptions{..} = response where
    response = [Producer producerHandler]
    producerHandler raiseEvent = do
        let opt = _uoNextUciOptions!!i
            newActiveOpts = _uoActiveUciOptions & ix i .~ opt
        putMVar requestMVar $ buildUciRequest opt
        raiseEvent $ EventSetActiveOptions newActiveOpts

setActiveOptionsHandle :: [OptionUCI] -> EventHandle sp ep
setActiveOptionsHandle v model = [Model $ model & activeUciOptions .~ v]

buildUI :: UIBuilder UCIOptions OptionsEvent
buildUI _ UCIOptions{..} = tree where
    tree = vstack_ [childSpacing_ 16] optionWidgets
    optionWidgets = zipWith makeWidgetWithApplyButton [0..] _uoNextUciOptions
    makeWidgetWithApplyButton i opt = hstack_ [childSpacing_ 16]
        [ applyButton i `styleBasic` [sizeReqH $ fixedSize 24]
        , makeWidget i opt
        ]
    makeWidget i (SpinUCI caption value minVal maxVal) = hgrid'
        [ label $ pack caption <> ": " <> showt value
        , hslider_ (ixopt i . spinValue) minVal maxVal [dragRate 1]
        ]
    makeWidget i (ComboUCI caption _ values) = hgrid'
        [ label $ pack caption
        , textDropdown (ixopt i . comboValue) values
        ]
    makeWidget i (CheckUCI caption _) = hgrid'
        [ labeledCheckbox_ (pack caption) (ixopt i . checkValue) [textRight]
        ]
    makeWidget i (StringUCI caption _) = hgrid'
        [ label $ pack caption
        , textField $ ixopt i . stringValue
        ]
    makeWidget i (ButtonUCI caption) = button (pack caption) $ EventApply i
    applyButton i = button ">" (EventApply i) `nodeEnabled` diff where
        diff = nextValue /= activeValue
        nextValue = showValue $ _uoNextUciOptions!!i
        activeValue = showValue $ _uoActiveUciOptions!!i
    hgrid' = hgrid_ [childSpacing_ 16]
    ixopt :: Int -> Lens' UCIOptions OptionUCI
    ixopt i = nextUciOptions . ixl i
    ixl :: Int -> Lens' [OptionUCI] OptionUCI
    ixl i = lens (!!i) (\x v -> x & ix i .~ v)
