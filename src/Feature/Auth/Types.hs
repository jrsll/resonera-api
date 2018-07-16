module Feature.Auth.Types where

import ClassyPrelude
import Platform.AesonUtil
import           Database.PostgreSQL.Simple.FromRow

type Token  = Text
type UserId = Integer

data CurrentUser = CurrentUser
  { currentUserToken  :: Token
  , currentUserId     :: UserId
  , currentUserClaims :: [UserClaim] }

data UserClaim = UserClaim
  { userClaimName  :: Text
  , userClaimValue :: Text
  } deriving (Eq, Show)

data TokenError
  = TokenErrorUserIdNotFound
  | TokenErrorNotFound
  | TokenErrorExpired
  | TokenErrorMalformed String
  deriving (Eq, Show)

-- Instances

instance FromRow UserClaim where
  fromRow = UserClaim <$> field <*> field

$(commonJSONDeriveMany
  [ ''TokenError
  , ''UserClaim
  ])