module Feature.Claim.HTTP
      ( routes
      , Service(..)
      ) where

import           ClassyPrelude             hiding (delete)

import           Data.Aeson                (eitherDecode)
import qualified Data.ByteString.Lazy      as LB
import           Feature.Auth.Types
import           Feature.Claim.Types
import           Feature.Common.HTTP
import           Network.HTTP.Types.Status
import           Text.Digestive.Form       ((.:))
import qualified Text.Digestive.Form       as DF
import qualified Text.Digestive.Types      as DF
import           Text.Read                 (read)
import           Web.Scotty.Trans

class Monad m => Service m where
  addClaim :: Claim -> m (Either ClaimError ())
  removeClaim :: Claim -> m (Either ClaimError ())
  getClaimsForUser :: UserId -> m (Either ClaimError [Claim])

routes :: (Service m, MonadIO m) => ScottyT LText m ()
routes = do

  -- admin

  get "/api/admin/claims/:userId" $ do
    uId <- param "userId"
    result <- stopIfError claimErrorHandler $ getClaimsForUser uId
    json $ ClaimWrapper result

  post "/api/admin/claims" $ do
    claim <- parseJsonBody ("claim" .: claimForm)
    result <- stopIfError claimErrorHandler $ addClaim claim
    json $ ClaimWrapper result

  delete "/api/admin/claims" $ do
    claim <- parseJsonBody ("claim" .: claimForm)
    result <- stopIfError claimErrorHandler $ removeClaim claim
    json $ ClaimWrapper result

-- * Errors

claimErrorHandler :: (ScottyError e, Monad m) => ClaimError -> ActionT e m ()
claimErrorHandler err = do
    status status500
    json err

-- * Request deserialization & validation

claimNameValidation :: Text -> DF.Result [Text] ClaimName
claimNameValidation txt =
  case eitherDecode bs of
    Left err -> DF.Error [pack err]
    Right cn -> DF.Success cn
  where bs = LB.fromStrict $ encodeUtf8 txt

claimValueValidation :: Text -> DF.Result [Text] ClaimValue
claimValueValidation txt =
  case eitherDecode bs of
    Left err -> DF.Error [pack err]
    Right cn -> DF.Success cn
  where bs = LB.fromStrict $ encodeUtf8 txt

claimForm :: (Monad m) => DF.Form [Text] m Claim
claimForm = Claim <$> "name"   .: DF.validate claimNameValidation (DF.text Nothing)
                  <*> "value"  .: DF.validate claimValueValidation (DF.text Nothing)
                  <*> "userId" .: fmap read (DF.string Nothing)
