module Feature.Auth.JWT where

import           ClassyPrelude
import           Control.Monad.Except
import qualified Data.Aeson            as Aeson
import           Data.Has
import qualified Data.Text             as T
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Feature.Auth.Types
import           Jose.Jwa
import           Jose.Jwt
import           Platform.JWT
import           Text.Read             (read)

resolveToken :: (JWT r m) => Token -> m (Either TokenError CurrentUser)
resolveToken token = runExceptT $ do
  jwks      <- asks $ envJwks . getter
  eitherJwt <- lift $ decode jwks (Just $ JwsEncoding RS256) (encodeUtf8 token)
  curTime   <- liftIO getPOSIXTime
  (userId, userClaims) <- either throwError return $ do
    Jws (_, claimsRaw) <- first (TokenErrorMalformed . show) eitherJwt
    jwtClaims <- first TokenErrorMalformed $ Aeson.eitherDecode $ fromStrict claimsRaw
    let (IntDate expiredAt) = fromMaybe (IntDate curTime) $ jwtExp jwtClaims
    when (expiredAt < curTime) $ Left TokenErrorExpired
    uid <- maybe (Left TokenErrorUserIdNotFound) Right $ jwtSub jwtClaims >>= readMay
    uc <- maybe (Right []) Right $ decodeUserClaims =<< jwtJti jwtClaims
    return (uid, uc)
  return $ CurrentUser token userId userClaims

decodeUserClaims :: Text -> Maybe [UserClaim]
decodeUserClaims s = Aeson.decodeStrict (read $ T.unpack s)
