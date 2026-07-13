{-# LANGUAGE OverloadedStrings #-}

module Bus.Validation.Aeson (parseObject, parseObjectM) where

import Bus.Util.MessageCode (
    errorValidationRequiredJsonArrayItem,
    errorValidationRequiredJsonKey,
    errorValidationUnknownJsonError,
    errorValidationValidJsonInteger,
    errorValidationValidJsonSyntax,
    errorValidationValidJsonType,
 )
import Bus.Validation.Error (ValidationError (..))
import Data.Aeson (Object, Value (Object))
import Data.Aeson.BetterErrors (ErrorSpecifics (..), JSONType (..), Parse, ParseError (..), ParseT, PathPiece (..), parseValue, parseValueM)
import Data.Aeson.Text (encodeToLazyText)
import Data.Bifunctor (Bifunctor (first))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import TextShow (TextShow (showt))

import Data.HashMap.Strict qualified as Map
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text

parseObject :: Parse (NonEmpty ValidationError) a -> Object -> Either (NonEmpty ValidationError) a
parseObject parser object = first toValidationErrors (parseValue parser (Object object))

parseObjectM :: (Monad m) => ParseT (NonEmpty ValidationError) m a -> Object -> m (Either (NonEmpty ValidationError) a)
parseObjectM parser object = first toValidationErrors <$> (parseValueM parser (Object object))

toValidationErrors :: ParseError (NonEmpty ValidationError) -> NonEmpty ValidationError
toValidationErrors = \case
    InvalidJSON err -> validationError Nothing (Text.pack err) errorValidationValidJsonSyntax [] :| []
    BadSchema paths err ->
        let field = displayPath paths
         in case err of
                KeyMissing key ->
                    validationError
                        field
                        ("Missing key \"" <> key <> "\"")
                        errorValidationRequiredJsonKey
                        [("key", key)]
                        :| []
                OutOfBounds index ->
                    let index' = showt index
                     in validationError
                            field
                            ("Array index out of bound: " <> index')
                            errorValidationRequiredJsonArrayItem
                            [("index", index')]
                            :| []
                WrongType jsonType value ->
                    let jsonType' = displayJsonType jsonType
                        value' = toStrict (encodeToLazyText value)
                     in validationError
                            field
                            ("Expected type " <> jsonType' <> ", but got " <> value')
                            errorValidationValidJsonType
                            [("jsonType", jsonType'), ("value", value')]
                            :| []
                ExpectedIntegral num ->
                    let num' = showt num
                     in validationError
                            field
                            ("Expected integer, but got " <> num')
                            errorValidationValidJsonInteger
                            [("nuumber", num')]
                            :| []
                FromAeson msg ->
                    validationError
                        field
                        (Text.pack msg)
                        errorValidationUnknownJsonError
                        []
                        :| []
                CustomError valErrs -> valErrs
  where
    validationError field msg code args =
        ValidationError
            { valField = field
            , valMessage = msg
            , valMessageCode = code
            , valMessageArgs = Map.fromList args
            }

displayPath :: [PathPiece] -> Maybe Text
displayPath = \case
    [] -> Nothing
    (path : paths) -> case (path :| paths) >>= toText of
        "." :| ps -> Just (Text.concat ps)
        ps -> Just (Text.concat (NonEmpty.toList ps))
  where
    toText = \case
        ObjectKey key -> "." :| [key]
        ArrayIndex index -> "[" :| [showt index, "]"]

displayJsonType :: JSONType -> Text
displayJsonType = \case
    TyObject -> "object"
    TyArray -> "array"
    TyString -> "string"
    TyNumber -> "number"
    TyBool -> "boolean"
    TyNull -> "null"
