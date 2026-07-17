{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Bus.Validation.Error (ValidationError (..), requestValidationException) where

import Bus.Exception (ApiException (..), etyInvalidRequestParameters)
import Bus.Util.Aeson (fieldPrefixRemovalOptions)
import Data.Aeson (
    FromJSON (parseJSON),
    ToJSON (toEncoding, toJSON),
    genericParseJSON,
    genericToEncoding,
    genericToJSON,
 )
import Data.HashMap.Strict (HashMap)
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
    parseJSON = genericParseJSON (fieldPrefixRemovalOptions "val")

instance ToJSON ValidationError where
    toJSON = genericToJSON (fieldPrefixRemovalOptions "val")
    toEncoding = genericToEncoding (fieldPrefixRemovalOptions "val")

requestValidationException :: Maybe Text -> NonEmpty ValidationError -> ApiException
requestValidationException description errors =
    ApiException
        { apiHttpStatus = status400
        , apiErrorType = etyInvalidRequestParameters
        , apiErrorDescription = description
        , apiErrorDetails = Just (KeyMap.singleton "violations" (toJSON errors))
        }
