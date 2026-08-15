{-# LANGUAGE OverloadedStrings #-}

module Bus.Web.User.Api (registerUser, getUser) where

import Bus.Database.Class (MonadDatabase (runBeam), withTransaction)
import Bus.Database.Entity (BusTrackerDb (btUser), busTrackerDb)
import Bus.Logger (logDebug')
import Bus.Security.Jwt (Token)
import Bus.Web.App.Type (AppM)
import Data.Aeson (Object)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.UUID (UUID)
import Database.Beam (MonadIO (liftIO), all_, runSelectReturningList, select)

import Bus.Web.User.Service qualified as UserSvc
import Data.Aeson.KeyMap qualified as KeyMap
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as Text

registerUser :: Object -> AppM (HashMap Text UUID)
registerUser user = do
    logDebug' ["New user account: ", Text.pack (show (KeyMap.lookup "account" user))]

    userId <- UserSvc.validateNewUser user >>= UserSvc.save
    pure (HashMap.singleton "userId" userId)

getUser :: Token -> AppM Int
getUser _ = do
    xs <- withTransaction $ \conn ->
        runBeam conn $ do
            runSelectReturningList (select (all_ (btUser busTrackerDb)))

    liftIO $ print xs

    pure 1
