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
    | AppRotateBoard
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
    AppRotateBoard -> rotateBoardHandle model
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

rotateBoardHandle :: EventHandle
rotateBoardHandle model =
    [ Model $ model & boardRotated %~ not
    , Event AppSyncBoard
    ]

syncBoardHandle :: EventHandle
syncBoardHandle model = [Model $ model & boardState .~ state] where
    state = getBoardState r $ model ^. chessPosition
    r = model ^. boardRotated

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
    ply = getPromotedPly model info Queen

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
        [ Model $ model & nextPly %~ ((`promoteTo` pieceType) <$>)
        , Event $ AppSetPromotionMenu False
        , Event AppRunNextPly
        ]
