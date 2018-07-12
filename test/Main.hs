import           ClassyPrelude
import           Control.Concurrent
import           Database.PostgreSQL.Simple
import qualified Lib
import qualified Misc.Client                as RW
import           System.Environment
import           Test.Hspec

import qualified Spec.Article               as Article
import qualified Spec.Comment               as Comment
import           Spec.Common
import qualified Spec.User                  as User

main :: IO ()
main = withEnv . hspec $ do
  User.spec
  Article.spec
  Comment.spec

withEnv :: IO () -> IO ()
withEnv = bracket startEnv cleanEnv . const

startEnv :: IO ThreadId
startEnv = do
  execPGQuery ["drop database if exists resonera_test", "create database resonera_test"]
  setEnv "DATABASE_URL" "postgresql://127.0.0.1/resonera_test"
  setEnv "ENABLE_HTTPS" "False"
  setEnv "JWT_EXPIRATION_SECS" "8"
  tId <- forkIO Lib.main
  unlessM healthCheck $ do
    putStrLn "Waiting for server ..."
    threadDelay 1000000
  return tId
  where
    healthCheck =
      either (const False) id <$> runClient RW.health

cleanEnv :: ThreadId -> IO ()
cleanEnv tId = do
  killThread tId
  putStrLn $ "Sever killed (" <> tshow tId <> ")"

execPGQuery :: [Query] -> IO ()
execPGQuery qrys =
  bracket acquire release execQuery
  where
    acquire = connectPostgreSQL "postgresql://127.0.0.1"
    release = close
    execQuery conn = forM_ qrys (void . execute_ conn)
