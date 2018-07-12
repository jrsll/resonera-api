module Feature.Claim.PG where

import ClassyPrelude
import Feature.Claim.Types
import Feature.Auth.Types
import Platform.PG
import Database.PostgreSQL.Simple

-- * ClaimRepo

getClaimsForUser :: (PG r m) => UserId -> m (Either ClaimError [Claim])
getClaimsForUser uid =
  do claims <- withConn $ \conn -> query conn qry (Only uid)
     return $ Right claims
  where
    qry = "select cast (name as text), cast (value as text), user_id from claims where user_id = ?"

addUserClaim :: (PG r m) => Claim -> m (Either ClaimError ())
addUserClaim c =
  do result <- withConn $ \conn -> try $ execute conn qry (claimName c, claimValue c, claimUserId c)
     return $ case result of
      Left (err :: SqlError) ->
        error $ "Unhandled PG error: " <> show err
      Right _ ->
        Right ()
  where
    qry = "insert into claims (name, value, user_id) values (?, ?, ?) on conflict do update"

removeUserClaim :: (PG r m) => Claim -> m (Either ClaimError ())
removeUserClaim c = do
  result <- withConn $ \conn -> try $ execute conn qry (claimName c, claimValue c, claimUserId c)
  return $ case result of
    Left (err :: SqlError) ->
      error $ "Unhandled PG error: " <> show err
    Right _ ->
      Right ()
  where
    qry = "delete from claims where name = ? and value = ? and user_id = ?"
