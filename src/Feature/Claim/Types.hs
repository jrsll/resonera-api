module Feature.Claim.Types where

import           ClassyPrelude
import           Data.Aeson
import           Data.Binary.Builder                  (putStringUtf8)
import qualified Data.ByteString.Lazy                 as LB
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Platform.AesonUtil
import           Text.Read                            (lexP, readEither,
                                                       readMaybe, readPrec)
import qualified Text.Read.Lex                        as L

newtype ClaimWrapper a = ClaimWrapper { claimWrapperClaim :: a } deriving (Eq, Show)

data ClaimName =
    User
  | Article
  deriving (Eq)

data ClaimValue =
    Read
  | Write
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
      Article -> "Article"
      User    -> "User"

instance Show ClaimValue where
  show cn =
    case cn of
      Read  -> "Read"
      Write -> "Write"

instance Read ClaimName where
  readPrec = do
    L.Ident s <- lexP
    case s of
      "Article" -> return Article
      "User"    -> return User
      _         -> fail $ "Could not parse " ++ s ++ " into type ClaimName."

instance Read ClaimValue where
  readPrec = do
    L.Ident s <- lexP
    case s of
      "Read"  -> return Read
      "Write" -> return Write
      _       -> fail $ "Could not parse " ++ s ++ " into type ClaimValue."

instance ToField ClaimName where
  toField =
    Plain . putStringUtf8 . show

instance ToField ClaimValue where
  toField =
    Plain . putStringUtf8 . show

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
  toJSON = String . fromString . show

instance ToJSON ClaimName where
  toJSON = String . fromString . show

instance FromJSON ClaimValue where
  parseJSON value =
    case value of
        String s  -> either fail return (readEither (show s))
        x         -> fail $ "Failed to parse JSON value " ++ show x ++ " to type ClaimValue, expected a string."

instance FromJSON ClaimName where
  parseJSON value =
    case value of
      String s  -> either fail return (readEither (show s))
      x         -> fail $ "Failed to parse JSON value " ++ show x ++ " to type ClaimName, expected a string."
