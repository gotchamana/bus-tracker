{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Bus.Validation.Rerefined (Trimmed, NotEmpty, Length, ValidPath, NetworkPort, refineField) where

import Bus.Util.MessageCode (
    errorValidationLength,
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
import Data.Proxy (Proxy (Proxy))
import Data.String (IsString (fromString))
import Data.Text (Text, pattern Empty, pattern (:<), pattern (:>))
import Data.Text.Builder.Linear (fromText, runBuilder)
import GHC.TypeLits (KnownNat, Natural, natVal)
import Rerefined
import Rerefined.Predicate
import Rerefined.Predicate.Common (validateFail)
import System.OsPath (OsPath, decodeUtf, isValid)
import TextShow (TextShow (showt))
import Valida (Validation, fromEither)

import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as Text
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

data Length (min :: Natural) (max :: Natural)

instance Predicate (Length min max) where
    type PredicateName d (Length min max) = "Length"

instance (KnownNat min, KnownNat max) => Refine (Length min max) Text where
    validate p text =
        if len >= lenMin && len <= lenMax
            then Nothing
            else validateFail p valErr []
      where
        lenMin = natVal (Proxy :: Proxy min)
        lenMin' = showt lenMin
        lenMax = natVal (Proxy :: Proxy max)
        lenMax' = showt lenMax
        len = fromIntegral (Text.length text)
        msg = Text.concat ["String length must be between ", lenMin', " and ", lenMax']
        valErr =
            fromText . LazyText.toStrict . encodeToLazyText $
                defaultValidationError' msg errorValidationLength [("min", lenMin'), ("max", lenMax')]

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
        , valMessageArgs = HashMap.empty
        }

defaultValidationError' :: Text -> Text -> [(Text, Text)] -> ValidationError
defaultValidationError' msg code args =
    ValidationError
        { valField = Nothing
        , valMessage = msg
        , valMessageCode = code
        , valMessageArgs = HashMap.fromList args
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
                , valMessageArgs = HashMap.empty
                }
            )
            (decodeStrictText text)
    setField v = v{valField = field}
