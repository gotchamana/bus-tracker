{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Bus.Validation.Rerefined (Trimmed, NotEmpty, ValidPath, NetworkPort, refineField) where

import Bus.Util.MessageCode (
    errorValidationNetworkPort,
    errorValidationNotEmpty,
    errorValidationTrimmed,
    errorValidationUnknownError,
    errorValidationValidPath,
 )
import Bus.Validation.Error (ValidationError (..))
import Data.Aeson (decodeStrictText)
import Data.Aeson.Text (encodeToLazyText)
import Data.Bifunctor (Bifunctor (first))
import Data.Char (isSpace)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromMaybe)
import Data.String (IsString (fromString))
import Data.Text (Text, pattern Empty, pattern (:<), pattern (:>))
import Data.Text.Builder.Linear (fromText, runBuilder)
import Rerefined
import Rerefined.Predicate
import Rerefined.Predicate.Common (validateFail)
import System.OsPath (OsPath, decodeUtf, isValid)
import TextShow (TextShow (showt))
import Valida (Validation, fromEither)

import Data.HashMap.Strict qualified as Map
import Data.Text.Lazy qualified as LazyText

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
                defaultValidationError "String is not trimmed" errorValidationTrimmed
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
                defaultValidationError "Empty string" errorValidationNotEmpty

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
                defaultValidationError ("Invalid file system path: " <> path') errorValidationValidPath

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
                defaultValidationError ("Invalid network port: " <> showt num) errorValidationNetworkPort

defaultValidationError :: Text -> Text -> ValidationError
defaultValidationError msg code =
    ValidationError
        { valField = Nothing
        , valMessage = msg
        , valMessageCode = code
        , valMessageArgs = Map.empty
        }

refineField :: (Refine p a) => Text -> a -> Validation (NonEmpty ValidationError) (Refined p a)
refineField field = fromEither . first (collectAsValidationErrors (Just field)) . refine

collectAsValidationErrors :: Maybe Text -> RefineFailure -> NonEmpty ValidationError
collectAsValidationErrors field RefineFailure{refineFailureDetail, refineFailureInner} =
    case refineFailureInner of
        [] -> (setField . toValidationError . runBuilder $ refineFailureDetail) :| []
        (x : xs) -> (x :| xs) >>= collectAsValidationErrors field
  where
    toValidationError text =
        fromMaybe
            ( ValidationError
                { valField = Nothing
                , valMessage = text
                , valMessageCode = errorValidationUnknownError
                , valMessageArgs = Map.empty
                }
            )
            (decodeStrictText text)
    setField v = v{valField = field}
