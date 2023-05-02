module Model.AppEvent
    ( AppEvent(..)
    , handleEvent
    ) where

import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Control.Lens
import Data.Maybe
import Data.Text (pack, unpack, Text)
import Game.Chess
import Game.Chess.SAN
import Monomer
import System.Random
import TextShow

import Model.AppModel

data AppEvent
    = AppInit
    | AppSetPosition Position
    | AppRotateBoard
    | AppSyncBoard
    | AppBoardChanged ([[Piece]], Int, Int)
    | AppEditBoardChanged ([[Piece]], Int, Int)
    | AppSetEditMenu Bool
    | AppSetPromotionMenu Bool
    | AppSetErrorMessage (Maybe Text)
    | AppRunNextPly
    | AppPromote PieceType
    | AppPlayNextResponse
    | AppSetThinkingAnimation Int
    | AppResponseCalculated (Maybe Ply, StdGen, Maybe Int)
    | AppUndoMove
    | AppLoadFEN
    | AppApplyEditChanges
    | AppClearEditBoard
    | AppUpdateFEN
    deriving (Eq, Show)

type EventHandle = AppModel -> [AppEventResponse AppModel AppEvent]

instance NFData Ply where
    rnf x = x `seq` ()

handleEvent :: AppEventHandler AppModel AppEvent
handleEvent _ _ model event = case event of
    AppInit -> []
    AppSetPosition v -> setPositionHandle v model
    AppRotateBoard -> rotateBoardHandle model
    AppSyncBoard -> syncBoardHandle model
    AppBoardChanged info -> boardChangedHandle info model
    AppEditBoardChanged info -> editBoardChangedHandle info model
    AppSetEditMenu v -> setEditMenuHandle v model
    AppSetPromotionMenu v -> setPromotionMenuHandle v model
    AppSetErrorMessage v -> setErrorMessageHandle v model
    AppRunNextPly -> runNextPlyHandle model
    AppPromote pieceType -> promoteHandle pieceType model
    AppPlayNextResponse -> playNextResponseHandle model
    AppSetThinkingAnimation v -> setThinkingAnimationHandle v model
    AppResponseCalculated v -> responseCalculatedHandle v model
    AppUndoMove -> undoMoveHandle model
    AppLoadFEN -> loadFENHandle model
    AppApplyEditChanges -> applyEditChangesHandle model
    AppClearEditBoard -> clearEditBoardHandle model
    AppUpdateFEN -> updateFenHandle model

setPositionHandle :: Position -> EventHandle
setPositionHandle position model =
    [ Model $ model
        & chessPosition .~ position
        & previousPositions .~ []
        & showPromotionMenu .~ False
        & minimaxEvaluation .~ Nothing
        & sanMoves .~ ""
        & forsythEdwards .~ pack (toFEN position)
    , Event AppSyncBoard
    ]

rotateBoardHandle :: EventHandle
rotateBoardHandle model =
    [ Model $ model
        & boardRotated %~ not
        & boardState %~ reverse
        & fenData . fenBoardState %~ reverse
    ]

syncBoardHandle :: EventHandle
syncBoardHandle model = response where
    response =
        [ Model $ model
            & boardState .~ getBoardState r position
            & fenData .~ getFenData r position
        ]
    r = model ^. boardRotated
    position = model ^. chessPosition

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

editBoardChangedHandle :: ([[Piece]], Int, Int) -> EventHandle
editBoardChangedHandle (_, ixTo, ixFrom) model = response where
    response =
        [ responseIf (ixFrom >= 1000) $ Model $ modify model
        , Event AppUpdateFEN
        ]
    modify = fenData . fenBoardState . element ixTo .~ piece
    piece = chessPieces!!(ixFrom-1000)

setEditMenuHandle :: Bool -> EventHandle
setEditMenuHandle v model =
    [ Model $ model
        & showEditMenu .~ v
        & showPromotionMenu .~ False
        & errorMessage .~ Nothing
    ]

setPromotionMenuHandle :: Bool -> EventHandle
setPromotionMenuHandle v model =
    [ Model $ model & showPromotionMenu .~ v
    , responseIf v $ Event AppSyncBoard
    ]

setErrorMessageHandle :: Maybe Text -> EventHandle
setErrorMessageHandle v model =
    [ Model $ model
        & showPromotionMenu .~ False
        & errorMessage .~ v
    ]

runNextPlyHandle :: EventHandle
runNextPlyHandle model = response where
    response = if null newPosition
        then []
        else
            [ Model $ model
                & chessPosition .~ fromJust newPosition
                & previousPositions %~ ((currentPosition, moves):)
                & showPromotionMenu .~ False
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
        , Event $ AppSetThinkingAnimation 0
        , Task taskHandler
        ]
    taskHandler = do
        let result = calculateMove model
        result `deepseq` pure ()
        return $ AppResponseCalculated result

setThinkingAnimationHandle :: Int -> EventHandle
setThinkingAnimationHandle v model = response where
    response = if model ^. calculatingResponse
        then
            [ Model $ model & thinkingAnimation .~ animation
            , Task taskHandler
            ]
        else []
    animation = animations!!(v `mod` length animations)
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
        return $ AppSetThinkingAnimation $ v+1

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
                & showPromotionMenu .~ False
                & minimaxEvaluation .~ Nothing
                & sanMoves .~ moves
                & forsythEdwards .~ pack (toFEN previousPosition)
            , Event AppSyncBoard
            ]
    (previousPosition, moves) = head positions
    positions = model ^. previousPositions

loadFENHandle :: EventHandle
loadFENHandle model = [Task taskHandler] where
    taskHandler = do
        let newPosition = fromFEN $ unpack $ model ^. forsythEdwards
            x = fromJust newPosition
        catch (x `seq` return (AppSetPosition x)) f
    f :: ErrorCall -> IO AppEvent
    f = const $ return $ AppSetErrorMessage $ Just "Invalid FEN"

applyEditChangesHandle :: EventHandle
applyEditChangesHandle model =
    [ Model $ model & showEditMenu .~ False
    , Event AppLoadFEN
    ]

clearEditBoardHandle :: EventHandle
clearEditBoardHandle model =
    [ Model $ model & fenData . fenBoardState .~ take 64 (repeat [])
    , Event AppUpdateFEN
    ]

updateFenHandle :: EventHandle
updateFenHandle model = response where
    response = [Model $ model & forsythEdwards .~ newFEN]
    newFEN = pack $ toFEN $ fromJust $ fromFEN fenString
    fenString = getFenString r $ model ^. fenData
    r = model ^. boardRotated
