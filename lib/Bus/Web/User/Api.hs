module Bus.Web.User.Api (UserApi, userApi) where

import Bus.App (AppM)
import Bus.Database (BusTrackerDb (btUser), MonadDatabase (runBeam, withTransactionMode), busTrackerDb)
import Bus.Exception (ApiException (..), etyInvalidRequestParameters)
import Bus.Logging (logDebug, logDebug')
import Control.Monad.Catch (MonadThrow (throwM))
import Data.Aeson (Object)
import Data.UUID (UUID)
import Data.Void (Void)
import Database.Beam (MonadIO (liftIO), all_, runSelectReturningList, select)
import Database.PostgreSQL.Simple.Transaction (defaultTransactionMode)
import Network.HTTP.Types.Status (status400)
import Servant

import Bus.Web.User.Service qualified as UserSvc
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text

type UserApi = "users" :> (ReqBody '[JSON] Object :> Post '[JSON] UUID :<|> Get '[JSON] Int)

userApi :: ServerT UserApi AppM
userApi = registerUser :<|> getUser

registerUser :: Object -> AppM UUID
registerUser user = do
    case UserSvc.validateNewUser user of
        Left err -> logDebug' (NonEmpty.toList $ Text.pack . show <$> err)
        Right u -> logDebug (Text.pack (show u))

    -- UserSvc.save user

    throwM
        ApiException
            { apiHttpStatus = status400
            , apiErrorType = etyInvalidRequestParameters
            , apiErrorDescription = Nothing
            , apiErrorDetails = Nothing :: Maybe Void
            }

getUser :: AppM Int
getUser = do
    xs <- withTransactionMode defaultTransactionMode $ \conn ->
        runBeam conn $ do
            runSelectReturningList (select (all_ (btUser busTrackerDb)))

    liftIO $ print xs

    pure 1
