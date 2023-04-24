module Model.AppEvent
    ( AppEvent(..)
    , handleEvent
    ) where

import Control.Lens
import Data.Maybe
import Game.Chess
import Monomer

import Model.AppModel

data AppEvent
    = AppInit
    | AppResetBoard
    | AppSyncBoard
    | AppBoardChanged ([[Piece]], Int, Int)
    | AppSetPromotionMenu Bool
    | AppRunNextPly
    | AppPromote PieceType
    deriving (Eq, Show)

type EventHandle = AppModel -> [AppEventResponse AppModel AppEvent]

handleEvent :: AppEventHandler AppModel AppEvent
handleEvent _ _ model event = case event of
    AppInit -> []
    AppResetBoard -> resetBoardHandle model
    AppSyncBoard -> syncBoardHandle model
    AppBoardChanged info -> boardChangedHandle info model
    AppSetPromotionMenu v -> setPromotionMenuHandle v model
    AppRunNextPly -> runNextPlyHandle model
    AppPromote pieceType -> promoteHandle pieceType model

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
    response = if model ^. autoQueen
        then
            [ setNextPly
            , Event AppRunNextPly
            ]
        else
            [ setNextPly
            , Event $ if null (plyPromotion ply)
                then AppRunNextPly
                else AppSetPromotionMenu True
            ]
    setNextPly = Model $ model & nextPly .~ Just ply
    ply = promotePly model (getPly info) Queen

setPromotionMenuHandle :: Bool -> EventHandle
setPromotionMenuHandle v model =
    [ Model $ model & showPromotionMenu .~ v
    , Event AppSyncBoard
    ]

runNextPlyHandle :: EventHandle
runNextPlyHandle model = response where
    response = if null newPosition
        then []
        else
            [ Model $ model & chessPosition .~ fromJust newPosition
            , Event AppSyncBoard
            ]
    newPosition = unsafeDoPly (model ^. chessPosition) <$> ply
    ply = model ^. nextPly

promoteHandle :: PieceType -> EventHandle
promoteHandle pieceType model = response where
    response =
        [ Model $ model & nextPly .~ promotedPly
        , Event $ AppSetPromotionMenu False
        , Event AppRunNextPly
        ]
    promotedPly = flip (promotePly model) pieceType <$> ply
    ply = model ^. nextPly
