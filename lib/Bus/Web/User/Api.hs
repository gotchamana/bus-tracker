{-# LANGUAGE OverloadedStrings #-}

module Bus.Web.User.Api (UserApi, userApi) where

import Bus.App (AppM)
import Bus.Database (MonadDatabase (withConnection))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Database.PostgreSQL.Simple (Only, query_)
import Servant
import Control.Exception (throwIO, ArithException (Overflow))

type UserApi = "users" :> Get '[JSON] Int

userApi :: ServerT UserApi AppM
userApi = getUser

getUser :: AppM Int
getUser = do
    xs <- withConnection $ \conn -> throwIO Overflow >> query_ @(Only Int) conn "select 1 + 1"

    liftIO $ print xs
    pure 1
