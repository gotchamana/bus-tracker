{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Bus.Web.User.Service (NewUser (..), save, validateNewUser) where

import Bus.Database (MonadDatabase)
import Bus.Database.Table.User (UserT (..))
import Bus.Validation.Aeson (parseObject)
import Bus.Validation.Error (ValidationError)
import Bus.Validation.Rerefined (NotEmpty, Trimmed, refineField)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (Object)
import Data.Aeson.BetterErrors (asText, key, throwCustomError)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Time (ZonedTime (zonedTimeToLocalTime), getZonedTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Rerefined (Refined, unrefine)
import Rerefined.Predicates (And)
import Valida (Validation (..))

import Bus.Database.Repository.User qualified as UserRepo

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
