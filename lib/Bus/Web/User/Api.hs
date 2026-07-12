module Bus.Web.User.Api (UserApi, userApi) where

import Bus.App (AppM)
import Bus.Database (BusTrackerDb (btUser), MonadDatabase (runBeam, withTransactionMode), busTrackerDb)
import Data.Aeson (Object)
import Data.UUID (UUID)
import Database.Beam (MonadIO (liftIO), all_, runSelectReturningList, select)
import Database.PostgreSQL.Simple.Transaction (defaultTransactionMode)
import Servant

import Bus.Web.User.Service qualified as UserSvc

type UserApi = "users" :> (ReqBody '[JSON] Object :> Post '[JSON] UUID :<|> Get '[JSON] Int)

userApi :: ServerT UserApi AppM
userApi = registerUser :<|> getUser

registerUser :: Object -> AppM UUID
registerUser user = do
    UserSvc.validateNewUser user >>= UserSvc.save

getUser :: AppM Int
getUser = do
    xs <- withTransactionMode defaultTransactionMode $ \conn ->
        runBeam conn $ do
            runSelectReturningList (select (all_ (btUser busTrackerDb)))

    liftIO $ print xs

    pure 1
