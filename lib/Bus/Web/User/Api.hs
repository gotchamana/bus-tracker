module Bus.Web.User.Api (UserApi, userApi) where

import Bus.App (AppM)
import Bus.Database (BusTrackerDb (btUser), MonadDatabase (runBeam, withTransactionMode), busTrackerDb)
import Bus.Exception (ApiException (..), etyInvalidRequestParameters)
import Bus.Logging (logDebug)
import Bus.Web.User.Service (NewUser (usrAccount))
import Control.Monad.Catch (MonadThrow (throwM))
import Data.Aeson (Value)
import Data.UUID (UUID)
import Database.Beam (MonadIO (liftIO), all_, runSelectReturningList, select)
import Database.PostgreSQL.Simple.Transaction (defaultTransactionMode)
import Network.HTTP.Types.Status (status400)
import Servant

import Bus.Web.User.Service qualified as UserSvc

type UserApi = "users" :> (ReqBody '[JSON] Value :> Post '[JSON] UUID :<|> Get '[JSON] Int)

userApi :: ServerT UserApi AppM
userApi = registerUser :<|> getUser

registerUser :: Value -> AppM UUID
registerUser user = do
    -- logDebug (usrAccount user)
    -- UserSvc.save user

    throwM
        ApiException
            { apiHttpStatus = status400
            , apiErrorType = etyInvalidRequestParameters
            , apiErrorDescription = Nothing
            , apiErrorDetails = Nothing
            }

getUser :: AppM Int
getUser = do
    xs <- withTransactionMode defaultTransactionMode $ \conn ->
        runBeam conn $ do
            runSelectReturningList (select (all_ (btUser busTrackerDb)))

    liftIO $ print xs

    pure 1
