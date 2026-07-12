{-# LANGUAGE OverloadedStrings #-}

module Bus.Validation.Aeson (parseObject) where

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
import Data.Aeson.BetterErrors (ErrorSpecifics (..), JSONType (..), Parse, ParseError (..), parseValue)
import Data.Aeson.Text (encodeToLazyText)
import Data.Bifunctor (Bifunctor (first))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text.Lazy (toStrict)
import TextShow (TextShow (showt))

import Data.HashMap.Strict qualified as Map
import Data.Text qualified as Text

parseObject :: Parse (NonEmpty ValidationError) a -> Object -> Either (NonEmpty ValidationError) a
parseObject parser object = first toValidationErrors (parseValue parser (Object object))

toValidationErrors :: ParseError (NonEmpty ValidationError) -> NonEmpty ValidationError
toValidationErrors = \case
    InvalidJSON err -> defaultValidationError (Text.pack err) errorValidationValidJsonSyntax [] :| []
    BadSchema _ err -> case err of
        KeyMissing key ->
            defaultValidationError
                ("Missing key \"" <> key <> "\"")
                errorValidationRequiredJsonKey
                [("key", key)]
                :| []
        OutOfBounds index ->
            let index' = showt index
             in defaultValidationError
                    ("Array index out of bound: " <> index')
                    errorValidationRequiredJsonArrayItem
                    [("index", index')]
                    :| []
        WrongType jsonType value ->
            let jsonType' = displayJsonType jsonType
                value' = toStrict (encodeToLazyText value)
             in defaultValidationError
                    ("Expected type " <> jsonType' <> ", but got " <> value')
                    errorValidationValidJsonType
                    [("jsonType", jsonType'), ("value", value')]
                    :| []
        ExpectedIntegral num ->
            let num' = showt num
             in defaultValidationError
                    ("Expected integer, but got " <> num')
                    errorValidationValidJsonInteger
                    [("nuumber", num')]
                    :| []
        FromAeson msg ->
            defaultValidationError
                (Text.pack msg)
                errorValidationUnknownJsonError
                []
                :| []
        CustomError valErrs -> valErrs
  where
    defaultValidationError msg code args =
        ValidationError
            { valField = Nothing
            , valMessage = msg
            , valMessageCode = code
            , valMessageArgs = Map.fromList args
            }
    displayJsonType = \case
        TyObject -> "object"
        TyArray -> "array"
        TyString -> "string"
        TyNumber -> "number"
        TyBool -> "boolean"
        TyNull -> "null"
