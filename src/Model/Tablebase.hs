{-# LANGUAGE RecordWildCards #-}

module Model.Tablebase
    ( TablebaseData(..)
    , defaultTablebaseData
    , tablebaseRequestAnalysis
    ) where

import Control.Exception
import Control.Lens
import Data.Aeson
import Data.Text (pack, Text)
import Game.Chess
import Network.HTTP.Client (HttpException(..))
import Network.Wreq
import TextShow

data MoveCategory = Winning | Drawing | Losing

data MoveResponse = MoveResponse
    { _mrUci :: String
    , _mrSan :: String
    , _mrDistanceToZero :: Maybe Int
    , _mrDistanceToMate :: Maybe Int
    , _mrZeroing :: Bool
    , _mrCheckmate :: Bool
    , _mrStalemate :: Bool
    , _mrInsufMaterial :: Bool
    , _mrCategory :: String
    }

instance FromJSON MoveResponse where
    parseJSON = withObject "MoveResponse" $ \v -> MoveResponse
        <$> v .: "uci"
        <*> v .: "san"
        <*> v .: "dtz"
        <*> v .: "dtm"
        <*> v .: "zeroing"
        <*> v .: "checkmate"
        <*> v .: "stalemate"
        <*> v .: "insufficient_material"
        <*> v .: "category"

data TotalResponse = TotalResponse
    { _trCheckmate :: Bool
    , _trStalemate :: Bool
    , _trInsufMaterial :: Bool
    , _trCategory :: String
    , _trMoves :: [MoveResponse]
    }

instance FromJSON TotalResponse where
    parseJSON = withObject "TotalResponse" $ \v -> TotalResponse
        <$> v .: "checkmate"
        <*> v .: "stalemate"
        <*> v .: "insufficient_material"
        <*> v .: "category"
        <*> v .: "moves"

type MoveLine = (Text, Text, Maybe Ply)

data TablebaseData = TablebaseData
    { _tbdStatusMessage :: Maybe Text
    , _tbdWinMoves :: [MoveLine]
    , _tbdDrawMoves :: [MoveLine]
    , _tbdLoseMoves :: [MoveLine]
    } deriving (Eq, Show)

defaultTablebaseData :: TablebaseData
defaultTablebaseData = TablebaseData
    { _tbdStatusMessage = Nothing
    , _tbdWinMoves = []
    , _tbdDrawMoves = []
    , _tbdLoseMoves = []
    }

tablebaseRequestAnalysis :: Position -> IO TablebaseData
tablebaseRequestAnalysis position = processResponse <$> try' (get url) where
    processResponse x = case x of
        Left (HttpExceptionRequest _ exc) -> tbError $ pack $ show exc
        Right response -> maybe nd ttd $ decode $ response ^. responseBody
    url = "http://tablebase.lichess.ovh/standard?fen=" <> toFEN position
    try' = try :: IO a -> IO (Either HttpException a)
    nd = tbError "Can't decode the JSON data"
    ttd = toTablebaseData position

tbError :: Text -> TablebaseData
tbError message = defaultTablebaseData
    { _tbdStatusMessage = Just message
    }

toTablebaseData :: Position -> TotalResponse -> TablebaseData
toTablebaseData position TotalResponse{..} = result where
    result = TablebaseData
        { _tbdStatusMessage = Just message
        , _tbdWinMoves = winMoves
        , _tbdDrawMoves = drawMoves
        , _tbdLoseMoves = loseMoves
        }
    message
        | _trCheckmate && isWhite = "White won by checkmate"
        | _trCheckmate = "Black won by checkmate"
        | _trStalemate = "Draw by stalemate"
        | _trInsufMaterial = "Draw by insufficient material"
        | _trCategory == "win" && isWhite = "White is winning"
        | _trCategory == "win" = "Black is winning"
        | _trCategory == "unknown" = "Position not found in the tablebase"
        | _trCategory == "maybe-win" && isWhite = "White is winning (maybe)"
        | _trCategory == "maybe-win" = "Black is winning (maybe)"
        | cursedWin && isWhite = "White is winning (cursed win)"
        | cursedWin = "Black is winning (cursed win)"
        | _trCategory == "draw" = "Tablebase draw"
        | blessedLoss && isWhite = "White is losing (blessed loss)"
        | blessedLoss = "Black is losing (blessed loss)"
        | _trCategory == "maybe-loss" && isWhite = "White is losing (maybe)"
        | _trCategory == "maybe-loss" = "Black is losing (maybe)"
        | _trCategory == "loss" && isWhite = "White is losing"
        | _trCategory == "loss" = "Black is losing"
        | otherwise = "Unknown category"
    cursedWin = _trCategory == "cursed-win"
    blessedLoss = _trCategory == "blessed-loss"
    isWhite = color position == White
    (winMoves, drawMoves, loseMoves) = distributeMoves position _trMoves

distributeMoves
    :: Position
    -> [MoveResponse]
    -> ([MoveLine], [MoveLine], [MoveLine])
distributeMoves _ [] = ([], [], [])
distributeMoves position (x:xs) = result where
    result = case categorizeMove x of
        Just Losing -> (moveLine:w, d, l)
        Just Drawing -> (w, moveLine:d, l)
        Just Winning -> (w, d, moveLine:l)
        Nothing -> otherMoves
    moveLine = toMoveLine position x
    otherMoves@(w, d, l) = distributeMoves position xs

categorizeMove :: MoveResponse -> Maybe MoveCategory
categorizeMove MoveResponse{..}
    | _mrCheckmate || _mrCategory `elem` lossCategories = Just Losing
    | _mrStalemate || _mrInsufMaterial || _mrCategory == "draw" = Just Drawing
    | _mrCategory `elem` winCategories = Just Winning
    | otherwise = Nothing
    where
        winCategories = ["win", "maybe-win", "cursed-win"]
        lossCategories = ["loss", "maybe-loss", "blessed-loss"]

toMoveLine :: Position -> MoveResponse -> MoveLine
toMoveLine position MoveResponse{..} = (pack _mrSan, info, ply) where
    info
        | _mrCheckmate = "Checkmate"
        | _mrStalemate = "Stalemate"
        | _mrInsufMaterial = "Insuf. mat."
        | _mrZeroing = "Zeroing/" <> dtm
        | otherwise = dtz <> "/" <> dtm
    dtz = maybe "NA" (showt . abs) _mrDistanceToZero
    dtm = maybe "NA" (showt . abs) _mrDistanceToMate
    ply = fromUCI position _mrUci
