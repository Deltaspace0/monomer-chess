module Model.AppEvent
    ( AppEvent(..)
    , handleEvent
    ) where

import Control.Lens
import Game.Chess
import Monomer

import Model.AppModel

data AppEvent
    = AppInit
    | AppResetBoard
    | AppSyncBoard
    | AppBoardChanged ([[Piece]], Int, Int)
    deriving (Eq, Show)

type EventHandle = AppModel -> [AppEventResponse AppModel AppEvent]

handleEvent :: AppEventHandler AppModel AppEvent
handleEvent _ _ model event = case event of
    AppInit -> []
    AppResetBoard -> resetBoardHandle model
    AppSyncBoard -> syncBoardHandle model
    AppBoardChanged info -> boardChangedHandle info model

resetBoardHandle :: EventHandle
resetBoardHandle model =
    [ Model $ model & chessPosition .~ startpos
    , Event AppSyncBoard
    ]

syncBoardHandle :: EventHandle
syncBoardHandle model = [Model $ model & boardState .~ state] where
    state = getBoardState $ model ^. chessPosition

boardChangedHandle :: ([[Piece]], Int, Int) -> EventHandle
boardChangedHandle info model = response where
    response =
        [ Model $ model & chessPosition .~ newPosition
        , Event AppSyncBoard
        ]
    newPosition = unsafeDoPly (model ^. chessPosition) ply
    ply = promotePly model (getPly info) Queen
