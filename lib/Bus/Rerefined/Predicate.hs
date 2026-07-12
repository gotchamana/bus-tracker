{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Bus.Rerefined.Predicate (NotEmpty, Trimmed, ValidPath, NetworkPort, ValidationError (..)) where

import Data.Aeson (FromJSON (parseJSON), Options (fieldLabelModifier), ToJSON (toEncoding, toJSON), defaultOptions, genericParseJSON, genericToEncoding, genericToJSON)
import Data.Aeson.Text (encodeToLazyText)
import Data.Char (isSpace, toLower)
import Data.HashMap.Strict (HashMap)
import Data.List (stripPrefix)
import Data.String (IsString (fromString))
import Data.Text (Text, pattern Empty, pattern (:<), pattern (:>))
import Data.Text.Builder.Linear (fromText)
import GHC.Generics (Generic)
import Rerefined.Predicate (Predicate (PredicateName), Refine (validate))
import Rerefined.Predicate.Common (validateFail)
import System.OsPath (OsPath, decodeUtf, isValid)
import TextShow (TextShow (showt))

import Data.HashMap.Strict qualified as Map
import Data.Text.Lazy qualified as LazyText

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

data Trimmed

instance Predicate Trimmed where
    type PredicateName d Trimmed = "Trimmed"

instance Refine Trimmed Text where
    validate p = \case
        Empty -> Nothing
        (x :< xs) -> do
            if isSpace x
                then err
                else case xs of
                    Empty -> Nothing
                    (_ :> x') ->
                        if isSpace x'
                            then err
                            else Nothing
      where
        valErr =
            fromText . LazyText.toStrict . encodeToLazyText $
                defaultValidationError "String is not trimmed" "error.validation.trimmed"
        err = validateFail p valErr []

data NotEmpty

instance Predicate NotEmpty where
    type PredicateName d NotEmpty = "NotEmpty"

instance Refine NotEmpty Text where
    validate p = \case
        Empty -> validateFail p valErr []
        _ -> Nothing
      where
        valErr =
            fromText . LazyText.toStrict . encodeToLazyText $
                defaultValidationError "Empty string" "error.validation.not-empty"

data ValidPath

instance Predicate ValidPath where
    type PredicateName d ValidPath = "ValidPath"

instance Refine ValidPath OsPath where
    validate p path =
        if isValid path
            then Nothing
            else validateFail p valErr []
      where
        path' = fromString $ case decodeUtf path of
            Just fp -> fp
            Nothing -> show path
        valErr =
            fromText . LazyText.toStrict . encodeToLazyText $
                defaultValidationError ("Invalid file system path: " <> path') "error.validation.valid-path"

data NetworkPort

instance Predicate NetworkPort where
    type PredicateName d NetworkPort = "NetworkPort"

instance Refine NetworkPort Int where
    validate p num =
        if num < 0 || num > 65535
            then validateFail p valErr []
            else Nothing
      where
        valErr =
            fromText . LazyText.toStrict . encodeToLazyText $
                defaultValidationError ("Invalid network port: " <> showt num) "error.validation.network-port"

defaultValidationError :: Text -> Text -> ValidationError
defaultValidationError msg code =
    ValidationError
        { valField = Nothing
        , valMessage = msg
        , valMessageCode = code
        , valMessageArgs = Map.empty
        }

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
