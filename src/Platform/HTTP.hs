module Platform.HTTP
      ( main
      ) where

import ClassyPrelude

import Web.Scotty.Trans
import Network.HTTP.Types.Status
import Network.Wai (Response)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.Wai.Middleware.Cors

import qualified Feature.Auth.HTTP as Auth
import qualified Feature.Claim.HTTP as Claim
import qualified Feature.User.HTTP as User
import qualified Feature.Comment.HTTP as Comment
import qualified Feature.Article.HTTP as Article

import System.Environment

type App r m = (Article.Service m, Claim.Service m, Auth.Service m, Comment.Service m, User.Service m, MonadIO m)

main :: (App r m) => (m Response -> IO Response) -> IO ()
main runner = do
  port <- acquirePort
  mayTLSSetting <- acquireTLSSetting
  case mayTLSSetting of
    Nothing ->
      scottyT port runner routes
    Just tlsSetting -> do
      app <- scottyAppT runner routes
      runTLS tlsSetting (setPort port defaultSettings) app
  where
    acquirePort = do
      port <- fromMaybe "" <$> lookupEnv "PORT"
      return . fromMaybe 3000 $ readMay port
    acquireTLSSetting = do
      env <- (>>= readMay) <$> lookupEnv "ENABLE_HTTPS"
      let enableHttps = fromMaybe False env
      return $ if enableHttps
        then Just $ tlsSettings "secrets/tls/certificate.pem" "secrets/tls/key.pem"
        else Nothing



-- * Routing

routes :: (App r m) => ScottyT LText m ()
routes = do
  -- middlewares
  middleware $ cors $ const $ Just simpleCorsResourcePolicy
    { corsRequestHeaders = "Authorization":simpleHeaders
    , corsMethods = "PUT":"DELETE":simpleMethods
    }
  options (regex ".*") $ return ()

  -- err 
  defaultHandler $ \str -> do
    status status500
    json str

  -- feature routes
  User.routes
  Article.routes
  Comment.routes
  Claim.routes
  
  -- health
  get "/api/health" $
    json True
