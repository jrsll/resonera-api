module Feature.Article.Service where

import           ClassyPrelude
import           Control.Monad.Except
import           Data.Convertible      (convert)
import           Feature.Article.Types
import           Feature.Auth.Types
import           Feature.Common.Types
import           System.Posix.Types    (EpochTime)
import qualified Web.Slug              as WSlug

getArticles :: (ArticleRepo m) => Maybe CurrentUser -> ArticleFilter -> Pagination -> m [Article]
getArticles = findArticles Nothing Nothing

getFeed :: (ArticleRepo m) => CurrentUser -> Pagination -> m [Article]
getFeed curUser =
  findArticles Nothing (Just True) (Just curUser) (ArticleFilter Nothing Nothing Nothing)

getArticle :: (ArticleRepo m) => Maybe CurrentUser -> Slug -> m (Either ArticleError Article)
getArticle mayCurUser slug = runExceptT $ do
  result <- lift $ findArticles (Just slug) Nothing mayCurUser (ArticleFilter Nothing Nothing Nothing) (Pagination 1 0)
  case result of
    [article] -> return article
    _         -> throwError $ ArticleErrorNotFound slug

createArticle :: (ArticleRepo m, TimeRepo m) => CurrentUser -> CreateArticle -> m (Either ArticleError Article)
createArticle curUser param = do
  slug <- genSlug' (createArticleTitle param) (currentUserId curUser)
  addArticle (currentUserId curUser) param slug
  getArticle (Just curUser) slug

updateArticle :: (ArticleRepo m, TimeRepo m) => CurrentUser -> Slug -> UpdateArticle -> m (Either ArticleError Article)
updateArticle curUser slug param = runExceptT $ do
  ExceptT $ validateArticleOwnedBy (currentUserId curUser) slug
  newSlug <- case updateArticleTitle param of
    Nothing       -> return slug
    Just newTitle -> lift $ genSlug' newTitle (currentUserId curUser)
  lift $ updateArticleBySlug slug param newSlug
  ExceptT $ getArticle (Just curUser) newSlug

genSlug' :: (TimeRepo m) => Text -> Integer -> m Text
genSlug' title uId = genSlug title uId . convert <$> currentTime

genSlug :: Text -> Integer -> EpochTime -> Text
genSlug title userId unixTs =
  maybe "invalidSlug" WSlug.unSlug $ WSlug.mkSlug $ unwords [tshow userId, tshow unixTs, title]

deleteArticle :: (ArticleRepo m) => CurrentUser -> Slug -> m (Either ArticleError ())
deleteArticle curUser slug = runExceptT $ do
  ExceptT $ validateArticleOwnedBy (currentUserId curUser) slug
  lift $ deleteArticleBySlug slug

validateArticleOwnedBy :: (ArticleRepo m) => UserId -> Slug -> m (Either ArticleError ())
validateArticleOwnedBy uId slug = runExceptT $ do
  result <- lift $ isArticleOwnedBy uId slug
  case result of
    Nothing    -> throwError $ ArticleErrorNotFound slug
    Just False -> throwError $ ArticleErrorNotAllowed slug
    _          -> return ()

favoriteArticle :: (ArticleRepo m) => CurrentUser -> Slug -> m (Either ArticleError Article)
favoriteArticle curUser slug = do
  favoriteArticleBySlug (currentUserId curUser) slug
  getArticle (Just curUser) slug

unfavoriteArticle :: (ArticleRepo m) => CurrentUser -> Slug -> m (Either ArticleError Article)
unfavoriteArticle curUser slug = do
  unfavoriteArticleBySlug (currentUserId curUser) slug
  getArticle (Just curUser) slug

class (Monad m) => ArticleRepo m where
  findArticles :: Maybe Slug -> Maybe Bool -> Maybe CurrentUser
               -> ArticleFilter -> Pagination
               -> m [Article]
  addArticle :: UserId -> CreateArticle -> Slug -> m ()
  updateArticleBySlug :: Slug -> UpdateArticle -> Slug -> m ()
  deleteArticleBySlug :: Slug -> m ()
  favoriteArticleBySlug :: UserId -> Slug -> m ()
  unfavoriteArticleBySlug :: UserId -> Slug -> m ()
  isArticleOwnedBy :: UserId -> Slug -> m (Maybe Bool)
  isArticleExist :: Slug -> m Bool

class (Monad m) => TimeRepo m where
  currentTime :: m UTCTime

class (Monad m) => TagRepo m where
  allTags :: m (Set Tag)
