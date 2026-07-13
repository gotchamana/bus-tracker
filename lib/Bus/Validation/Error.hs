{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Bus.Validation.Error (ValidationError (..), requestValidationException) where

import Bus.Exception (ApiException (..), etyInvalidRequestParameters)
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
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Types.Status (status400)

import Data.Aeson.KeyMap qualified as KeyMap

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

requestValidationException :: Maybe Text -> NonEmpty ValidationError -> ApiException
requestValidationException description errors =
    ApiException
        { apiHttpStatus = status400
        , apiErrorType = etyInvalidRequestParameters
        , apiErrorDescription = description
        , apiErrorDetails = Just (KeyMap.singleton "violations" (toJSON errors))
        }
