module Feature.Auth.Types where

import           Database.PostgreSQL.Simple.FromRow
import           ClassyPrelude
import           Feature.Claim.Types (ClaimName, ClaimValue)
import           Platform.AesonUtil

type Token  = Text
type UserId = Integer

data CurrentUser = CurrentUser
  { currentUserToken  :: Token
  , currentUserId     :: UserId
  , currentUserClaims :: [UserClaim] }

data UserClaim = UserClaim
  { userClaimName  :: ClaimName
  , userClaimValue :: ClaimValue
  } deriving (Eq, Show)

data TokenError
  = TokenErrorUserIdNotFound
  | TokenErrorNotFound
  | TokenErrorExpired
  | TokenErrorMalformed String
  | TokenErrorMissingClaim UserClaim
  deriving (Eq, Show)

-- Instances

instance FromRow UserClaim where
  fromRow = UserClaim <$> field <*> field

$(commonJSONDeriveMany
  [ ''TokenError
  , ''UserClaim
  ])
