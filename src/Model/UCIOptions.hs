{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.UCIOptions
    ( OptionUCI(..)
    , UCIOptions(..)
    , spinValue
    , comboValue
    , checkValue
    , stringValue
    , activeUciOptions
    , nextUciOptions
    , initUciOptions
    , getChangedUciOptions
    , mergeUciOptions
    , buildUciRequest
    , showCaption
    , showValue
    , parseUciOption
    ) where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Types
import Data.Foldable
import Data.List (elemIndex, elemIndices)
import Data.Maybe
import Data.Text (pack, unpack, Text)

data OptionUCI
    = SpinUCI
        { _spuSpinCaption :: String
        , _spuSpinValue :: Int
        , _spuSpinMinValue :: Int
        , _spuSpinMaxValue :: Int
        }
    | ComboUCI
        { _couComboCaption :: String
        , _couComboValue :: String
        , _couComboValues :: [String]
        }
    | CheckUCI
        { _chuCheckCaption :: String
        , _chuCheckValue :: Bool
        }
    | StringUCI
        { _stuStringCaption :: String
        , _stuStringValue :: Text
        }
    | ButtonUCI
        { _buuButtonCaption :: String
        }
    deriving (Eq, Show)

data UCIOptions = UCIOptions
    { _uoActiveUciOptions :: [OptionUCI]
    , _uoNextUciOptions :: [OptionUCI]
    } deriving (Eq, Show)

instance FromJSON OptionUCI where
    parseJSON = withObject "OptionUCI" $ \v -> mconcat
        [ SpinUCI
            <$> v .: "spin_caption"
            <*> v .: "spin_value"
            <*> v .: "spin_min_value"
            <*> v .: "spin_max_value"
        , ComboUCI
            <$> v .: "combo_caption"
            <*> v .: "combo_value"
            <*> v .: "combo_values"
        , CheckUCI
            <$> v .: "check_caption"
            <*> v .: "check_value"
        , StringUCI
            <$> v .: "string_caption"
            <*> v .: "string_value"
        , ButtonUCI
            <$> v .: "button_caption"
        ]

instance ToJSON OptionUCI where
    toJSON SpinUCI{..} = object
        [ "spin_caption" .= _spuSpinCaption
        , "spin_value" .= _spuSpinValue
        , "spin_min_value" .= _spuSpinMinValue
        , "spin_max_value" .= _spuSpinMaxValue
        ]
    toJSON ComboUCI{..} = object
        [ "combo_caption" .= _couComboCaption
        , "combo_value" .= _couComboValue
        , "combo_values" .= _couComboValues
        ]
    toJSON CheckUCI{..} = object
        [ "check_caption" .= _chuCheckCaption
        , "check_value" .= _chuCheckValue
        ]
    toJSON StringUCI{..} = object
        [ "string_caption" .= _stuStringCaption
        , "string_value" .= _stuStringValue
        ]
    toJSON ButtonUCI{..} = object
        [ "button_caption" .= _buuButtonCaption
        ]

instance ToJSON UCIOptions where
    toJSON UCIOptions{..} = toJSON _uoNextUciOptions

spinValue :: Lens' OptionUCI Int
spinValue = lens _spuSpinValue (\x v -> x {_spuSpinValue = v})

comboValue :: Lens' OptionUCI String
comboValue = lens _couComboValue (\x v -> x {_couComboValue = v})

checkValue :: Lens' OptionUCI Bool
checkValue = lens _chuCheckValue (\x v -> x {_chuCheckValue = v})

stringValue :: Lens' OptionUCI Text
stringValue = lens _stuStringValue (\x v -> x {_stuStringValue = v})

makeLensesWith abbreviatedFields 'UCIOptions

initUciOptions :: [OptionUCI] -> UCIOptions
initUciOptions options = UCIOptions
    { _uoActiveUciOptions = options
    , _uoNextUciOptions = options
    }

instance FromJSON UCIOptions where
    parseJSON = withArray "UCIOptions" $
        fmap (initUciOptions . toList) . mapM parseJSON

getChangedUciOptions :: UCIOptions -> [OptionUCI]
getChangedUciOptions UCIOptions{..} = result where
    result = snd <$> filter (\(x, y) -> x /= y) optPairs
    optPairs = zip _uoActiveUciOptions _uoNextUciOptions

mergeUciOptions :: UCIOptions -> UCIOptions -> UCIOptions
mergeUciOptions newOpts oldOpts = resultOpts where
    resultOpts = UCIOptions
        { _uoActiveUciOptions = activeNewOpts
        , _uoNextUciOptions = zipWith mergeOpt activeNewOpts nextOldOpts
        }
    activeNewOpts = _uoActiveUciOptions newOpts
    nextOldOpts = _uoNextUciOptions oldOpts <> (repeat $ ButtonUCI "")
    mergeOpt x@(SpinUCI c1 _ a b) (SpinUCI c2 v _ _) = if c1 == c2
        then SpinUCI c1 v a b
        else x
    mergeOpt x@(ComboUCI c1 _ xs) (ComboUCI c2 v _) = if c1 == c2
        then ComboUCI c1 v xs
        else x
    mergeOpt x@(CheckUCI c1 _) (CheckUCI c2 v) = if c1 == c2
        then CheckUCI c1 v
        else x
    mergeOpt x@(StringUCI c1 _) (StringUCI c2 v) = if c1 == c2
        then StringUCI c1 v
        else x
    mergeOpt x _ = x

buildUciRequest :: OptionUCI -> String
buildUciRequest opt = result where
    result = "setoption name " <> (showCaption opt) <> valueText
    valueText = maybe "" (" value " <>) $ showValue opt

showCaption :: OptionUCI -> String
showCaption (SpinUCI caption _ _ _) = caption
showCaption (ComboUCI caption _ _) = caption
showCaption (CheckUCI caption _) = caption
showCaption (StringUCI caption _) = caption
showCaption (ButtonUCI caption) = caption

showValue :: OptionUCI -> Maybe String
showValue (SpinUCI _ value _ _) = Just $ show value
showValue (ComboUCI _ value _) = Just value
showValue (CheckUCI _ True) = Just "true"
showValue (CheckUCI _ False) = Just "false"
showValue (StringUCI _ value) = Just $ unpack value
showValue (ButtonUCI _) = Nothing

parseUciOption :: String -> Maybe OptionUCI
parseUciOption optionString = result where
    result = case stringParameterFromUci optionString "type" of
        Just "spin" -> parseSpinUci optionString
        Just "combo" -> parseComboUci optionString
        Just "check" -> parseCheckUci optionString
        Just "string" -> parseStringUci optionString
        Just "button" -> parseButtonUci optionString
        _ -> Nothing

parseSpinUci :: String -> Maybe OptionUCI
parseSpinUci optionString = do
    caption <- optionNameFromUci optionString
    defaultValue <- intParameterFromUci optionString "default"
    minValue <- intParameterFromUci optionString "min"
    maxValue <- intParameterFromUci optionString "max"
    return $ SpinUCI
        { _spuSpinCaption = caption
        , _spuSpinValue = defaultValue
        , _spuSpinMinValue = minValue
        , _spuSpinMaxValue = maxValue
        }

parseComboUci :: String -> Maybe OptionUCI
parseComboUci optionString = do
    caption <- optionNameFromUci optionString
    defaultValue <- stringParameterFromUci optionString "default"
    let values = getParametersFromUci optionString "var"
    return $ ComboUCI
        { _couComboCaption = caption
        , _couComboValue = defaultValue
        , _couComboValues = values
        }

parseCheckUci :: String -> Maybe OptionUCI
parseCheckUci optionString = do
    caption <- optionNameFromUci optionString
    defaultValue <- boolParameterFromUci optionString "default"
    return $ CheckUCI
        { _chuCheckCaption = caption
        , _chuCheckValue = defaultValue
        }

parseStringUci :: String -> Maybe OptionUCI
parseStringUci optionString = do
    caption <- optionNameFromUci optionString
    defaultValue <- stringParameterFromUci optionString "default"
    return $ StringUCI
        { _stuStringCaption = caption
        , _stuStringValue = pack defaultValue
        }

parseButtonUci :: String -> Maybe OptionUCI
parseButtonUci optionString = do
    caption <- optionNameFromUci optionString
    return $ ButtonUCI
        { _buuButtonCaption = caption
        }

optionNameFromUci :: String -> Maybe String
optionNameFromUci optionString = do
    let ws = words optionString
    i <- elemIndex "name" ws
    return $ unwords $ takeWhile (/= "type") $ drop (i+1) ws

stringParameterFromUci :: String -> String -> Maybe String
stringParameterFromUci optionString param = listToMaybe params where
    params = getParametersFromUci optionString param

intParameterFromUci :: String -> String -> Maybe Int
intParameterFromUci optionString param = do
    stringParam <- stringParameterFromUci optionString param
    if stringParam == ""
        then Nothing
        else return $ read stringParam

boolParameterFromUci :: String -> String -> Maybe Bool
boolParameterFromUci optionString param = result where
    result = case stringParameterFromUci optionString param of
        Just "true" -> Just True
        Just "false" -> Just False
        _ -> Nothing

getParametersFromUci :: String -> String -> [String]
getParametersFromUci optionString param = result where
    result = f <$> elemIndices param ws
    f i = if i == length ws-1
        then ""
        else ws!!(i+1)
    ws = words optionString
