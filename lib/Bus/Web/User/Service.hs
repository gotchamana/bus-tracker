{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bus.Web.User.Service (NewUser (..), save, validateNewUser) where

import Bus.Database (MonadDatabase)
import Bus.Database.Table.User (UserT (..))
import Bus.Rerefined.Predicate (NotEmpty, Trimmed)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (Value)
import Data.Aeson.BetterErrors (Parse, ParseError, key, parseValue, withText)
import Data.Text (Text)
import Data.Time (ZonedTime (zonedTimeToLocalTime), getZonedTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Rerefined (Refined, refine, unrefine)
import Rerefined.Predicate (RefineFailure)
import Rerefined.Predicates (And)

import Bus.Database.Repository.User qualified as UserRepo
import Data.Text qualified as Text

data NewUser = NewUser
    { usrAccount :: Refined (And Trimmed NotEmpty) Text
    , usrPassword :: Refined NotEmpty Text
    }
    deriving (Show)

validateNewUser :: Value -> Either (ParseError RefineFailure) NewUser
validateNewUser = parseValue parser
  where
    parser :: Parse RefineFailure NewUser
    parser = do
        usrAccount <- key "account" (withText (refine . Text.strip))
        usrPassword <- key "password" (withText refine)

        pure NewUser{..}

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
