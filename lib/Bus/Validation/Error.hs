{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Bus.Validation.Error (ValidationError (..)) where

import Data.Aeson (
    FromJSON (parseJSON),
    Options (fieldLabelModifier),
    ToJSON (toEncoding, toJSON),
    defaultOptions,
    genericParseJSON,
    genericToEncoding,
    genericToJSON,
 )
import Data.Char (toLower)
import Data.HashMap.Strict (HashMap)
import Data.List (stripPrefix)
import Data.Text (Text)
import GHC.Generics (Generic)

data ValidationError = ValidationError
    { valField :: Maybe Text
    , valMessage :: Text
    , valMessageCode :: Text
    , valMessageArgs :: HashMap Text Text
    }
    deriving (Show, Generic)

instance FromJSON ValidationError where
    parseJSON = genericParseJSON (customOptions "val")

instance ToJSON ValidationError where
    toJSON = genericToJSON (customOptions "val")
    toEncoding = genericToEncoding (customOptions "val")

customOptions :: String -> Options
customOptions fieldPrefix =
    defaultOptions
        { fieldLabelModifier = removePrefix
        }
  where
    removePrefix field = case stripPrefix fieldPrefix field of
        Just result -> case result of
            [] -> []
            (x : xs) -> toLower x : xs
        Nothing -> field
