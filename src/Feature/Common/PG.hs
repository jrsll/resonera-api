module Feature.Common.PG where

import           ClassyPrelude
import           Database.PostgreSQL.Simple

isUniqueConstraintsViolation :: SqlError -> ByteString -> Bool
isUniqueConstraintsViolation SqlError{sqlState = state, sqlErrorMsg = msg} constraintName =
  state == "23505" && constraintName `isInfixOf` msg

isForeignKeyViolation :: SqlError -> ByteString -> Bool
isForeignKeyViolation SqlError{sqlState = state, sqlErrorMsg = msg} constraintName =
  state == "23503" && constraintName `isInfixOf` msg
