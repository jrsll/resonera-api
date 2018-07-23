module Feature.Auth.HTTP where

import           ClassyPrelude
import           Control.Monad.Except
import           Feature.Auth.Types
import           Feature.Common.HTTP
import           Feature.Common.Util       (orThrow)
import           Network.HTTP.Types.Status
import           Web.Scotty.Trans

class Monad m => Service m where
  resolveToken :: Token -> m (Either TokenError CurrentUser)

getCurrentUser :: (Service m) => ActionT LText m (Either TokenError CurrentUser)
getCurrentUser = do
  mayHeaderVal <- header "Authorization"
  runExceptT $ do
    headerVal <- ExceptT $ pure mayHeaderVal `orThrow` TokenErrorNotFound
    let token = toStrict $ drop 6 headerVal
    ExceptT $ lift $ resolveToken token

optionalUser :: (Service m) => ActionT LText m (Maybe CurrentUser)
optionalUser =
  either (const Nothing) Just <$> getCurrentUser

requireClaim :: (Service m) => UserClaim -> ActionT LText m ()
requireClaim claim = do
  user <- requireUser
  case find (== claim) (currentUserClaims user) of
    Nothing -> tokenErrorHandler (TokenErrorMissingClaim claim)
    Just _  -> return ()
  where
    tokenErrorHandler e = do
      status status401
      json e
      finish

requireUser :: (Service m) => ActionT LText m CurrentUser
requireUser = do
  result <- getCurrentUser
  stopIfError tokenErrorHandler (pure result)
  where
    tokenErrorHandler e = do
      status status401
      json e
