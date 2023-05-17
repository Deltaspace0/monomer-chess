module Composites.ThinkButton
    ( thinkButton
    ) where

import Control.Concurrent
import Data.Text (Text)
import Monomer

data ThinkEvent = EventSetAnimation Int deriving (Eq, Show)

thinkButton
    :: (CompositeModel sp, CompositeEvent ep)
    => WidgetNode sp ep
thinkButton = node where
    node = compositeD_ wt wdata buildUI handleEvent cmpConfigs
    wt = "thinkButton"
    wdata = WidgetValue "42"
    cmpConfigs =
        [ onInit $ EventSetAnimation 0
        , compositeMergeModel $ \_ _ m _ -> m
        ]

handleEvent :: EventHandler Text ThinkEvent sp ep
handleEvent _ _ _ (EventSetAnimation i) = response where
    response =
        [ Model $ animations!!(i `mod` length animations)
        , Task taskHandler
        ]
    animations =
        [ "_.~\"~._.~\"~. Thinking _.~\"~._.~\"~."
        , "._.~\"~._.~\"~ Thinking ._.~\"~._.~\"~"
        , "~._.~\"~._.~\" Thinking ~._.~\"~._.~\""
        , "\"~._.~\"~._.~ Thinking \"~._.~\"~._.~"
        , "~\"~._.~\"~._. Thinking ~\"~._.~\"~._."
        , ".~\"~._.~\"~._ Thinking .~\"~._.~\"~._"
        ]
    taskHandler = do
        threadDelay 50000
        return $ EventSetAnimation $ i+1

buildUI :: UIBuilder Text ThinkEvent
buildUI _ thinkText = node `nodeEnabled` False where
    node = button thinkText $ EventSetAnimation 0
