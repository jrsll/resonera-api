module Feature.Claim.Types where

import ClassyPrelude

import Database.PostgreSQL.Simple.FromRow
import Platform.AesonUtil
import Feature.Auth.Types (UserId)

type ClaimName = Text
type ClaimValue = Text

newtype ClaimWrapper a = ClaimWrapper { claimWrapperClaim :: a } deriving (Eq, Show)

data Claim = Claim
  { claimName :: ClaimName
  , claimValue :: ClaimValue
  , claimUserId :: UserId
  } deriving (Eq, Show)

newtype ClaimError
  = ClaimErrorUserNotFound UserId
  deriving (Eq, Show)

-- * Instances

$(commonJSONDeriveMany
  [ ''Claim,
    ''ClaimError,
    ''ClaimWrapper
  ])

instance FromRow Claim where
  fromRow = Claim <$> field <*> field <*> field
