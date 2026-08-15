{-# LANGUAGE OverloadedStrings #-}

module Bus.Web.User.Api (registerUser, getUser) where

import Bus.Logger (logDebug')
import Bus.Security.Jwt (Token)
import Bus.Web.App.Type (AppM)
import Data.Aeson (Object)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.UUID (UUID)

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
    pure 1
