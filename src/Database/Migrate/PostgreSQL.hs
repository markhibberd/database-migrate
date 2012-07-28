{-#LANGUAGE OverloadedStrings #-}
module Database.Migrate.PostgreSQL where

import Control.Exception (SomeException(..), handle)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either

import Data.Text hiding (filter, reverse)
import Data.String (IsString(..))

import Database.PostgreSQL.Simple
import Database.Migrate.Core

instance MigrateDatabase IO Connection where
  initialize c = void $ execute_ c "CREATE TABLE IF NOT EXISTS MIGRATION_INFO (MIGRATION VARCHAR(50) PRIMARY KEY)"
  runMigrations = runall
  getMigrations c = fmap (fmap fromOnly) (query_ c "SELECT MIGRATION FROM MIGRATION_INFO")

record :: Connection -> MigrationId -> IO ()
record conn mid = void $ execute conn "INSERT INTO MIGRATION_INFO VALUES (?)" (Only mid)

runall :: Connection -> (Migration -> Ddl) -> [Migration] -> MigrationResultT IO [MigrationId]
runall c f ms =
  liftIO (begin c) >>
    (foldM (\rs m ->
             EitherT $
               do e <- runEitherT (saferun c f m)
                  case e of
                    Left emsg -> rollback c >> (return . Left $ Context (reverse rs) (migration m) emsg True)
                    Right r -> return . Right $ r:rs) [] ms) >>= \result -> liftIO (commit c) >> return (reverse result)

saferun :: Connection -> (Migration -> Ddl) -> Migration -> EitherT Text IO MigrationId
saferun c f m = EitherT $ handle (\e -> return (Left (pack . show $ (e :: SomeException)))) (fmap Right $ run c f m)

run :: Connection -> (Migration -> Ddl) -> Migration -> IO MigrationId
run c f m = execute_ c (fromString . unpack $ f m) >> record c (migration m) >> return (migration m)
