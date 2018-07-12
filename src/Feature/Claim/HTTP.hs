module Feature.Claim.HTTP
      ( routes
      , Service(..)
      ) where

import           ClassyPrelude             hiding (delete)

import           Feature.Auth.Types
import           Feature.Claim.Types
import           Feature.Common.HTTP
import           Network.HTTP.Types.Status
import           Text.Digestive.Form       ((.:))
import qualified Text.Digestive.Form       as DF
import qualified Text.Digestive.Types      as DF
import           Text.Read                 (read)
import           Text.Regex
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

matchesRegex :: v -> String -> Text -> DF.Result v Text
matchesRegex errMsg regexStr str =
  if isJust . matchRegex (mkRegexWithOpts regexStr True True) . unpack $ str
    then DF.Success str
    else DF.Error errMsg

minLength :: MonoFoldable a => Int -> a -> DF.Result Text a
minLength n str = if length str >= n then DF.Success str else DF.Error $ "Minimum length is " <> tshow n

maxLength :: MonoFoldable a => Int -> a -> DF.Result Text a
maxLength n str = if length str <= n then DF.Success str else DF.Error $ "Maximum length is " <> tshow n

claimNameValidation :: Text -> DF.Result [Text] Text
claimNameValidation = DF.conditions [minLength 3, maxLength 10, matchesRegex "Should be alphanumeric" "^[a-zA-Z0-9]+$"]

claimValueValidation :: Text -> DF.Result [Text] Text
claimValueValidation = DF.conditions [minLength 3, maxLength 15, matchesRegex "Should be alphanumeric" "^[a-zA-Z0-9]+$"]

claimForm :: (Monad m) => DF.Form [Text] m Claim
claimForm = Claim <$> "name"   .: DF.validate claimNameValidation (DF.text Nothing)
                  <*> "value"  .: DF.validate claimValueValidation (DF.text Nothing)
                  <*> "userId" .: fmap read (DF.string Nothing)
