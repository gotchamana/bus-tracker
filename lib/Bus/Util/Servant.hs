{-# LANGUAGE TypeFamilies #-}

module Bus.Util.Servant (WithAuth, AuthContext (..), KnownAuthType (..)) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader.Class (MonadReader (ask))
import Network.HTTP.Types.Header (hCookie)
import Network.Wai (Request (requestHeaders))
import Servant
import Servant.Server.Internal.Delayed (addAuthCheck)
import Servant.Server.Internal.DelayedIO (delayedFailFatal)
import Web.Cookie (Cookies, parseCookies)

data WithAuth (a :: [k]) t

newtype AuthContext t k = AuthContext
    { acAuthenticate :: k -> Maybe Cookies -> IO (Either ServerError t)
    }

class KnownAuthType (a :: k) where
    authType :: Proxy a -> k

instance (HasServer api ctx) => HasServer (WithAuth '[] t :> api) ctx where
    type ServerT (WithAuth '[] t :> api) m = ServerT api m

    hoistServerWithContext _ = hoistServerWithContext @api Proxy

    route _ = route @api Proxy

instance
    ( HasServer (WithAuth xs t :> api) ctx
    , HasContextEntry ctx (AuthContext t k)
    , KnownAuthType (x :: k)
    ) =>
    HasServer (WithAuth (x ': xs) t :> api) ctx
    where
    type ServerT (WithAuth (x ': xs) t :> api) m = t -> ServerT (WithAuth xs t :> api) m

    hoistServerWithContext _ ctx nt s = hoistServerWithContext @(WithAuth xs t :> api) Proxy ctx nt . s

    route _ ctx subserver = route @(WithAuth xs t :> api) Proxy ctx (addAuthCheck subserver check)
      where
        check = do
            request <- ask

            let authContext = getContextEntry ctx
                authType' = authType (Proxy @x)
                cookies = parseCookies <$> lookup hCookie (requestHeaders request)

            result <- liftIO (acAuthenticate authContext authType' cookies)

            case result of
                Left err -> delayedFailFatal err
                Right a -> pure a
