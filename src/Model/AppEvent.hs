module Model.AppEvent
    ( AppEvent(..)
    , handleEvent
    ) where

import Control.Lens
import Monomer

import Model.AppModel

data AppEvent
    = AppInit
    | AppResetBoard
    | AppBoardChanged ([[Piece]], Int, Int)
    deriving (Eq, Show)

type EventHandle = AppModel -> [AppEventResponse AppModel AppEvent]

handleEvent :: AppEventHandler AppModel AppEvent
handleEvent _ _ model event = case event of
    AppInit -> []
    AppResetBoard -> resetBoardHandle model
    AppBoardChanged info -> boardChangedHandle info model

resetBoardHandle :: EventHandle
resetBoardHandle model = response where
    response = [Model $ model & boardState .~ initBoard]
    initBoard = model ^. initBoardState

boardChangedHandle :: ([[Piece]], Int, Int) -> EventHandle
boardChangedHandle _ _ = []
