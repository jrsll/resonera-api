module Feature.User.Service where

import           ClassyPrelude
import           Control.Monad.Except
import           Feature.Auth.Types
import           Feature.Common.Util  (orThrow)
import           Feature.User.Types

-- * User

login :: (UserRepo m, TokenRepo m) => Auth -> m (Either UserError User)
login auth = runExceptT $ do
  (uId, user) <- ExceptT $ findUserByAuth auth `orThrow` UserErrorBadAuth auth
  token <- lift $ generateToken uId (userClaims user)
  return $ user { userToken = token }

register :: (UserRepo m, TokenRepo m) => Register -> m (Either UserError User)
register param@(Register _ email pass) = do
  let defaultImgUrl = "https://static.productionready.io/images/smiley-cyrus.jpg"
  result <- addUser param defaultImgUrl
  case result of
    Left err -> return . Left $ err
    Right _  -> login $ Auth email pass

getUser :: (UserRepo m) => CurrentUser -> m (Either UserError User)
getUser CurrentUser{ currentUserToken = token, currentUserId = userId } = runExceptT $ do
  user <- ExceptT $ findUserById userId `orThrow` UserErrorNotFound (tshow userId)
  return $ user { userToken = token }

updateUser :: (UserRepo m) => CurrentUser -> UpdateUser -> m (Either UserError User)
updateUser curUser param = runExceptT $ do
  ExceptT $ updateUserById (currentUserId curUser) param
  ExceptT $ getUser curUser

class (Monad m) => UserRepo m where
  findUserByAuth :: Auth -> m (Maybe (UserId, User))
  findUserById   :: UserId -> m (Maybe User)
  addUser        :: Register -> Text -> m (Either UserError ())
  updateUserById :: UserId -> UpdateUser -> m (Either UserError ())

class (Monad m) => TokenRepo m where
  generateToken :: UserId -> [UserClaim] -> m Token

-- * Profiles

getProfile :: (ProfileRepo m) => Maybe CurrentUser -> Username -> m (Either UserError Profile)
getProfile mayCurUser username =
  findProfile (currentUserId <$> mayCurUser) username `orThrow` UserErrorNotFound username

followUser :: (ProfileRepo m) => CurrentUser -> Username -> m (Either UserError Profile)
followUser curUser username = runExceptT $ do
  ExceptT $ followUserByUsername (currentUserId curUser) username
  ExceptT $ getProfile (Just curUser) username

unfollowUser :: (ProfileRepo m) => CurrentUser -> Username -> m (Either UserError Profile)
unfollowUser curUser username = do
  unfollowUserByUsername (currentUserId curUser) username
  getProfile (Just curUser) username

class (Monad m) => ProfileRepo m where
  findProfile            :: Maybe UserId -> Username -> m (Maybe Profile)
  followUserByUsername   :: UserId -> Username -> m (Either UserError ())
  unfollowUserByUsername :: UserId -> Username -> m ()
