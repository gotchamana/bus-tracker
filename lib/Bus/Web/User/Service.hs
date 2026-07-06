{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bus.Web.User.Service (NewUser (..), save) where

import Bus.Database (MonadDatabase)
import Bus.Database.Table.User (UserT (..))
import Bus.Rerefined.Predicate (NotEmpty, Trimmed)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (Options (fieldLabelModifier), Value, defaultOptions)
import Data.Aeson.BetterErrors (Parse, key, withText)
import Data.Char (toLower)
import Data.List (stripPrefix)
import Data.Text (Text)
import Data.Time (ZonedTime (zonedTimeToLocalTime), getZonedTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import GHC.Generics (Generic)
import Rerefined (Refined, refine, unrefine)
import Rerefined.Predicate (RefineFailure)
import Rerefined.Predicates (And)

import Bus.Database.Repository.User qualified as UserRepo

data NewUser = NewUser
    { usrAccount :: Refined (And Trimmed NotEmpty) Text
    , usrPassword :: Refined NotEmpty Text
    }
    deriving (Generic)

validateNewUser :: Value -> Either () NewUser
validateNewUser val = undefined
  where
    parser :: Parse RefineFailure NewUser
    parser = do
        usrAccount <- key "account" (withText refine)
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

customOptions :: String -> Options
customOptions fieldPrefix = defaultOptions{fieldLabelModifier = removePrefix}
  where
    removePrefix field = case stripPrefix fieldPrefix field of
        Just result -> case result of
            [] -> []
            (x : xs) -> toLower x : xs
        Nothing -> field
