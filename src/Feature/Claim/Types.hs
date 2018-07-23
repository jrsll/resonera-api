module Feature.Claim.Types where

import           ClassyPrelude
import           Data.Aeson
import qualified Data.ByteString                      as B
import qualified Data.ByteString.Lazy                 as LB
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow
import           Platform.AesonUtil
import           Text.Read                            (lexP, readEither,
                                                       readMaybe, readPrec)
import qualified Text.Read.Lex                        as L

newtype ClaimWrapper a = ClaimWrapper { claimWrapperClaim :: a } deriving (Eq, Show)

data ClaimName =
    UserClaimName
  | ArticleClaimName
  deriving (Eq)

data ClaimValue =
    ReadPermission
  | WritePermission
  deriving (Eq)

data Claim = Claim
  { claimName   :: ClaimName
  , claimValue  :: ClaimValue
  , claimUserId :: Text
  } deriving (Eq)

newtype ClaimError
  = ClaimErrorUserNotFound Text
  deriving (Eq, Show)

-- * Instances

$(commonJSONDeriveMany
  [ ''Claim,
    ''ClaimError,
    ''ClaimWrapper
  ])


instance Show ClaimName where
  show cn =
    case cn of
      ArticleClaimName -> "article"
      UserClaimName    -> "user"

instance Show ClaimValue where
  show cn =
    case cn of
      ReadPermission  -> "read"
      WritePermission -> "write"

instance Read ClaimName where
  readPrec = do
    L.Ident s <- lexP
    case s of
      "article" -> return ArticleClaimName
      "user"    -> return UserClaimName
      _         -> fail $ "Could not parse " ++ s ++ " into type ClaimName."

instance Read ClaimValue where
  readPrec = do
    L.Ident s <- lexP
    case s of
      "read"  -> return ReadPermission
      "write" -> return WritePermission
      _       -> fail $ "Could not parse " ++ s ++ " into type ClaimValue."

instance FromField ClaimName where
  fromField f mayDat =
    let mayCn = do dat <- mayDat
                   decode $ LB.fromStrict dat
    in case mayCn of
      Nothing -> returnError ConversionFailed f ""
      Just cn -> return cn

instance FromField ClaimValue where
  fromField f mayDat =
    case mayDat >>= (readMaybe . show) of
      Nothing -> returnError ConversionFailed f ""
      Just cv -> return cv

instance FromRow Claim where
  fromRow = Claim <$> field <*> field <*> field

instance ToJSON ClaimValue where
  toJSON x =
    case x of
      ReadPermission  -> String "read"
      WritePermission -> String "write"

instance FromJSON ClaimValue where
  parseJSON value =
    case value of
        String s  -> either fail return (readEither (show s))
        x         -> fail $ "Failed to parse JSON value " ++ show x ++ " to type ClaimValue, expected a string."

instance ToJSON ClaimName where
  toJSON x =
    case x of
      UserClaimName    -> String "user"
      ArticleClaimName -> String "article"

instance FromJSON ClaimName where
  parseJSON value =
    case value of
      String s  -> either fail return (readEither (show s))
      x         -> fail $ "Failed to parse JSON value " ++ show x ++ " to type ClaimName, expected a string."
