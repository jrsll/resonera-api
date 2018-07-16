module Feature.User.JWT where

import           ClassyPrelude
import qualified Data.Aeson            as Aeson
import           Data.Has              (getter)
import qualified Data.Text             as T
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Feature.Auth.Types
import           Jose.Jwa
import           Jose.Jwt
import           Platform.JWT

generateToken :: (JWT r m) => UserId -> [UserClaim] -> m Token
generateToken userId claims = do
  jwks <- asks $ envJwks . getter
  expirationSecs <- asks $ envExpirationSecs . getter
  curTime <- liftIO getPOSIXTime
  let jti = T.pack (show $ toStrict $ Aeson.encode claims)
  let claim = JwtClaims { jwtIss = Nothing
                        , jwtSub = Just $ tshow userId
                        , jwtAud = Nothing
                        , jwtExp = Just $ IntDate $ curTime + fromInteger expirationSecs
                        , jwtNbf = Nothing
                        , jwtIat = Nothing
                        , jwtJti = Just jti
                        }
  (Jwt jwtEncoded) <- either (\e -> error $ "Failed to encode JWT: " <> show e) id <$> encode jwks (JwsEncoding RS256) (Claims . toStrict . Aeson.encode $ claim)
  return . decodeUtf8 $ jwtEncoded
