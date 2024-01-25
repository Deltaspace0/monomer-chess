{-# LANGUAGE RecordWildCards #-}

module Model.AppEvent
    ( AppEvent(..)
    , handleEvent
    ) where

import Control.DeepSeq
import Control.Exception
import Control.Lens
import Data.Maybe
import Data.Text (pack, unpack, Text)
import Game.Chess
import Game.Chess.SAN
import Monomer
import TextShow

import Model.AppModel

data AppEvent
    = AppInit
    | AppSetPosition Position
    | AppSyncBoard
    | AppBoardChanged Bool ([[Piece]], Int, Int)
    | AppEditBoardChanged Bool ([[Piece]], Int, Int)
    | AppSetEditMenu Bool
    | AppSetPromotionMenu Bool
    | AppSetErrorMessage (Maybe Text)
    | AppRunNextPly
    | AppPromote PieceType
    | AppPlayNextResponse
    | AppResponseCalculated AIData
    | AppUndoMove
    | AppLoadFEN
    | AppApplyEditChanges
    | AppClearEditBoard
    | AppUpdateFEN
    deriving (Eq, Show)

type EventHandle = AppModel -> [AppEventResponse AppModel AppEvent]

handleEvent :: AppEventHandler AppModel AppEvent
handleEvent _ _ model event = case event of
    AppInit -> []
    AppSetPosition v -> setPositionHandle v model
    AppSyncBoard -> syncBoardHandle model
    AppBoardChanged r info -> boardChangedHandle r info model
    AppEditBoardChanged r info -> editBoardChangedHandle r info model
    AppSetEditMenu v -> setEditMenuHandle v model
    AppSetPromotionMenu v -> setPromotionMenuHandle v model
    AppSetErrorMessage v -> setErrorMessageHandle v model
    AppRunNextPly -> runNextPlyHandle model
    AppPromote pieceType -> promoteHandle pieceType model
    AppPlayNextResponse -> playNextResponseHandle model
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
        & sanMoves .~ ""
        & forsythEdwards .~ pack (toFEN position)
        & aiData . positionEvaluation .~ Nothing
    , Event AppSyncBoard
    ]

syncBoardHandle :: EventHandle
syncBoardHandle model@(AppModel{..}) = response where
    response =
        [ Model $ model
            & boardState .~ getBoardState False _amChessPosition
            & boardStateReversed .~ getBoardState True _amChessPosition
            & fenData .~ getFenData _amChessPosition
        ]

boardChangedHandle :: Bool -> ([[Piece]], Int, Int) -> EventHandle
boardChangedHandle r info model@(AppModel{..})
    | _amCalculatingResponse = [Event AppSyncBoard]
    | _amAutoQueen =
        [ setNextPly
        , Event AppRunNextPly
        , responseIf _amAutoRespond $ Event AppPlayNextResponse
        ]
    | otherwise =
        [ setNextPly
        , Event $ if noPromotion
            then AppRunNextPly
            else AppSetPromotionMenu True
        , responseIf (_amAutoRespond && noPromotion) $
            Event AppPlayNextResponse
        ]
    where
        setNextPly = Model $ model & nextPly .~ Just ply
        ply = getPromotedPly model r info Queen
        noPromotion = null $ plyPromotion ply

editBoardChangedHandle :: Bool -> ([[Piece]], Int, Int) -> EventHandle
editBoardChangedHandle r (_, ixTo, ixFrom) model = response where
    response =
        [ responseIf (ixFrom >= 1000) $ Model newModel
        , Model $ if r
            then interModel & fenData . fenBoardState .~
                reverse (interModel ^. fenData . fenBoardStateReversed)
            else interModel & fenData . fenBoardStateReversed .~
                reverse (interModel ^. fenData . fenBoardState)
        , Event AppUpdateFEN
        ]
    newModel = if r
        then model & fenData . fenBoardStateReversed . element ixTo .~ piece
        else model & fenData . fenBoardState . element ixTo .~ piece
    interModel = if ixFrom >= 1000
        then newModel
        else model
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
runNextPlyHandle model@(AppModel{..}) = response where
    response = if null newPosition || not isLegal
        then []
        else
            [ Model $ model
                & chessPosition .~ fromJust newPosition
                & previousPositions %~ ((_amChessPosition, moves):)
                & showPromotionMenu .~ False
                & sanMoves .~ newSanMoves
                & forsythEdwards .~ newFEN
            , Event AppSyncBoard
            ]
    isLegal = (fromJust _amNextPly) `elem` (legalPlies _amChessPosition)
    newFEN = pack $ toFEN $ fromJust newPosition
    newPosition = unsafeDoPly _amChessPosition <$> _amNextPly
    newSanMoves = moves <> numberText <> " " <> san
    moves = _amSanMoves
    numberText = if color _amChessPosition == White
        then (if moves == "" then "" else " ") <> number <> "."
        else ""
    number = showt $ moveNumber _amChessPosition
    san = pack $ unsafeToSAN _amChessPosition $ fromJust _amNextPly

promoteHandle :: PieceType -> EventHandle
promoteHandle pieceType model@(AppModel{..}) = response where
    response =
        [ Model $ model & nextPly %~ ((`promoteTo` pieceType) <$>)
        , Event $ AppSetPromotionMenu False
        , Event AppRunNextPly
        , responseIf _amAutoRespond $ Event AppPlayNextResponse
        ]

playNextResponseHandle :: EventHandle
playNextResponseHandle model@(AppModel{..}) = response where
    response =
        [ Model $ model & calculatingResponse .~ True
        , Task taskHandler
        ]
    taskHandler = do
        result <- calculateMove _amChessPosition _amAiData
        result `deepseq` pure ()
        return $ AppResponseCalculated result

responseCalculatedHandle :: AIData -> EventHandle
responseCalculatedHandle AIData{..} model =
    [ Model $ model
        & nextPly .~ _adResponsePly
        & calculatingResponse .~ False
        & aiData . positionEvaluation .~ _adPositionEvaluation
    , Event AppRunNextPly
    ]

undoMoveHandle :: EventHandle
undoMoveHandle model@(AppModel{..}) = response where
    response = if null _amPreviousPositions
        then []
        else
            [ Model $ model
                & chessPosition .~ previousPosition
                & previousPositions .~ tail _amPreviousPositions
                & showPromotionMenu .~ False
                & sanMoves .~ moves
                & forsythEdwards .~ pack (toFEN previousPosition)
                & aiData . positionEvaluation .~ Nothing
            , Event AppSyncBoard
            ]
    (previousPosition, moves) = head _amPreviousPositions

loadFENHandle :: EventHandle
loadFENHandle AppModel{..} = [Task taskHandler] where
    taskHandler = do
        let newPosition = fromFEN $ unpack _amForsythEdwards
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
    [ Model $ model
        & fenData . fenBoardState .~ take 64 (repeat [])
        & fenData . fenBoardStateReversed .~ take 64 (repeat [])
    , Event AppUpdateFEN
    ]

updateFenHandle :: EventHandle
updateFenHandle model@(AppModel{..}) = response where
    response = [Model $ model & forsythEdwards .~ newFEN]
    newFEN = pack $ toFEN $ fromJust $ fromFEN $ getFenString _amFenData
