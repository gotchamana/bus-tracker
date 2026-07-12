{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Bus.Web.User.Service (NewUser (..), save, validateNewUser) where

import Bus.Database (MonadDatabase)
import Bus.Database.Table.User (UserT (..))
import Bus.Rerefined.Predicate (NotEmpty, Trimmed, ValidationError (..), collectAsValidationErrors)
import Bus.Util.MessageCode (errorValidationRequiredJsonArrayItem, errorValidationRequiredJsonKey, errorValidationUnknownJsonError, errorValidationValidJsonInteger, errorValidationValidJsonSyntax, errorValidationValidJsonType)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (Object, Value (Object))
import Data.Aeson.BetterErrors (ErrorSpecifics (..), JSONType (..), Parse, ParseError (..), asText, key, parseValue, throwCustomError)
import Data.Aeson.Text (encodeToLazyText)
import Data.Bifunctor (Bifunctor (first))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Time (ZonedTime (zonedTimeToLocalTime), getZonedTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Rerefined (Refine, Refined, refine, unrefine)
import Rerefined.Predicates (And)
import TextShow (TextShow (showt))
import Valida (Validation (..), fromEither)

import Bus.Database.Repository.User qualified as UserRepo
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as Text

data NewUser = NewUser
    { usrAccount :: Refined (And Trimmed NotEmpty) Text
    , usrPassword :: Refined NotEmpty Text
    }
    deriving (Show)

validateNewUser :: Object -> Either (NonEmpty ValidationError) NewUser
validateNewUser = parseObject parser
  where
    parser = do
        account <- key "account" asText
        password <- key "password" asText

        let f =
                NewUser
                    <$> refineField "account" account
                    <*> refineField "password" password

        case f of
            Success n -> pure n
            Failure err -> throwCustomError err

refineField :: (Refine p a) => Text -> a -> Validation (NonEmpty ValidationError) (Refined p a)
refineField field = fromEither . first (collectAsValidationErrors (Just field)) . refine

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

save :: (MonadDatabase m) => NewUser -> m UUID
save user = do
    userId <- liftIO nextRandom
    now <- liftIO (zonedTimeToLocalTime <$> getZonedTime)

    UserRepo.save
        User
            { usrId = userId
            , usrAccount = unrefine user.usrAccount
            , usrPassword = unrefine user.usrPassword
            , usrCreateTime = now
            , usrUpdateTime = now
            }
    pure userId
