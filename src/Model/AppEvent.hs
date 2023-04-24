module Model.AppEvent
    ( AppEvent(..)
    , handleEvent
    ) where

import Control.Lens
import Data.Maybe
import Game.Chess
import Monomer
import System.Random

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
    | AppPlayRandomMove
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
    AppPlayRandomMove -> playRandomMoveHandle model

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
            , responseIf rand $ Event AppPlayRandomMove
            ]
        else
            [ setNextPly
            , Event $ if noPromotion
                then AppRunNextPly
                else AppSetPromotionMenu True
            , responseIf (rand && noPromotion) $
                Event AppPlayRandomMove
            ]
    setNextPly = Model $ model & nextPly .~ Just ply
    ply = getPromotedPly model info Queen
    noPromotion = null $ plyPromotion ply
    rand = model ^. autoRandom

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
        , responseIf (model ^. autoRandom) $ Event AppPlayRandomMove
        ]

playRandomMoveHandle :: EventHandle
playRandomMoveHandle model = response where
    response =
        [ Model $ model & nextPly .~ ply & randomGenerator .~ g
        , Event AppRunNextPly
        ]
    ply = if null legal
        then Nothing
        else Just $ legal!!i
    legal = legalPlies $ model ^. chessPosition
    (i, g) = randomR (0, length legal-1) $ model ^. randomGenerator
