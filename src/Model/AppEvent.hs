module Model.AppEvent
    ( AppEvent(..)
    , handleEvent
    ) where

import Control.DeepSeq
import Control.Lens
import Data.Maybe
import Data.Text (pack, unpack)
import Game.Chess
import Game.Chess.SAN
import Monomer
import System.Random
import TextShow

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
    | AppPlayNextResponse
    | AppResponseCalculated (Maybe Ply, StdGen, Maybe Int)
    | AppUndoMove
    | AppLoadFEN
    deriving (Eq, Show)

type EventHandle = AppModel -> [AppEventResponse AppModel AppEvent]

instance NFData Ply where
    rnf x = x `seq` ()

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
    AppPlayNextResponse -> playNextResponseHandle model
    AppResponseCalculated v -> responseCalculatedHandle v model
    AppUndoMove -> undoMoveHandle model
    AppLoadFEN -> loadFENHandle model

resetBoardHandle :: EventHandle
resetBoardHandle model =
    [ Model $ model
        & chessPosition .~ startpos
        & previousPositions .~ []
        & minimaxEvaluation .~ Nothing
        & sanMoves .~ ""
        & forsythEdwards .~ pack (toFEN startpos)
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
boardChangedHandle info model
    | model ^. calculatingResponse = [Event AppSyncBoard]
    | model ^. autoQueen =
        [ setNextPly
        , Event AppRunNextPly
        , responseIf resp $ Event AppPlayNextResponse
        ]
    | otherwise =
        [ setNextPly
        , Event $ if noPromotion
            then AppRunNextPly
            else AppSetPromotionMenu True
        , responseIf (resp && noPromotion) $
            Event AppPlayNextResponse
        ]
    where
        setNextPly = Model $ model & nextPly .~ Just ply
        ply = getPromotedPly model info Queen
        noPromotion = null $ plyPromotion ply
        resp = model ^. autoRespond

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
            [ Model $ model
                & chessPosition .~ fromJust newPosition
                & previousPositions %~ ((currentPosition, moves):)
                & sanMoves .~ newSanMoves
                & forsythEdwards .~ newFEN
            , Event AppSyncBoard
            ]
    newFEN = pack $ toFEN $ fromJust newPosition
    newPosition = unsafeDoPly currentPosition <$> ply
    newSanMoves = moves <> numberText <> " " <> san
    moves = model ^. sanMoves
    numberText = if color currentPosition == White
        then (if moves == "" then "" else " ") <> number <> "."
        else ""
    number = showt $ moveNumber currentPosition
    san = pack $ unsafeToSAN currentPosition $ fromJust ply
    currentPosition = model ^. chessPosition
    ply = model ^. nextPly

promoteHandle :: PieceType -> EventHandle
promoteHandle pieceType model = response where
    response =
        [ Model $ model & nextPly %~ ((`promoteTo` pieceType) <$>)
        , Event $ AppSetPromotionMenu False
        , Event AppRunNextPly
        , responseIf resp $ Event AppPlayNextResponse
        ]
    resp = model ^. autoRespond

playNextResponseHandle :: EventHandle
playNextResponseHandle model = response where
    response =
        [ Model $ model & calculatingResponse .~ True
        , Task taskHandler
        ]
    taskHandler = do
        let result = calculateMove model
        result `deepseq` pure ()
        return $ AppResponseCalculated result

responseCalculatedHandle
    :: (Maybe Ply, StdGen, Maybe Int)
    -> EventHandle
responseCalculatedHandle (ply, g, eval) model =
    [ Model $ model
        & nextPly .~ ply
        & randomGenerator .~ g
        & minimaxEvaluation .~ eval
        & calculatingResponse .~ False
    , Event AppRunNextPly
    ]

undoMoveHandle :: EventHandle
undoMoveHandle model = response where
    response = if null positions
        then []
        else
            [ Model $ model
                & chessPosition .~ previousPosition
                & previousPositions .~ tail positions
                & minimaxEvaluation .~ Nothing
                & sanMoves .~ moves
                & forsythEdwards .~ pack (toFEN previousPosition)
            , Event AppSyncBoard
            ]
    (previousPosition, moves) = head positions
    positions = model ^. previousPositions

loadFENHandle :: EventHandle
loadFENHandle model = response where
    response = if null newPosition
        then []
        else
            [ Model $ model
                & chessPosition .~ fromJust newPosition
                & previousPositions .~ []
                & minimaxEvaluation .~ Nothing
                & sanMoves .~ ""
                & forsythEdwards .~ newFEN
            , Event AppSyncBoard
            ]
    newFEN = pack $ toFEN $ fromJust newPosition
    newPosition = fromFEN $ unpack $ model ^. forsythEdwards
