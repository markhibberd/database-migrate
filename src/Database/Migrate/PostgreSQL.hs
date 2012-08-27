{-#LANGUAGE OverloadedStrings #-}
module Database.Migrate.PostgreSQL where

import Control.Exception (SomeException(..), handle)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either

import Data.Maybe
import Data.Text hiding (filter, reverse, find, null)
import Data.String (IsString(..))

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.Migrate.Core

instance FromField MigrationId where
  fromField f m = fmap MigrationId $ fromField f m

instance ToField MigrationId where
  toField (MigrationId m) = toField m

instance MigrateDatabase IO Connection where
  testconn c =  query_ c "SELECT TRUE" >>= \r -> return $ maybe False fromOnly (listToMaybe r)
  initialize c = void $ execute_ c "CREATE TABLE IF NOT EXISTS MIGRATION_INFO (MIGRATION VARCHAR(50) PRIMARY KEY)"
  initialized c = query_ c "SELECT TRUE FROM pg_tables WHERE schemaname='public' AND tablename = MIGRATION_INFO" >>= \r -> return $ maybe False fromOnly (listToMaybe r)
  runMigrations = runall
  getMigrations c = fmap (fmap fromOnly) (query_ c "SELECT MIGRATION FROM MIGRATION_INFO")


migratePostgres :: Connection -> FilePath -> (String -> IO ()) -> IO () -> IO ()
migratePostgres c path logger bomb = do
   initialize c
   ems <- runEitherT $ find path
   case ems of
     Left e -> logger e >> bomb
     Right ms -> runEitherT (latest c ms) >>= \er ->
       case er of
         Left (Context s f m r) ->
           forM_ s (\mid -> logger ("migration:applied: " ++ (unpack . extract $ mid))) >>
           logger ("migration:failed:" ++ (unpack . extract $ f) ++ ":" ++ unpack m) >>
           logger ("migration:rolledback:" ++ show r) >>
           bomb
         Right mids -> if null mids
                         then logger "migration:up-to-date"
                         else forM_ mids $ \mid -> logger $ "migration:applied: " ++ (unpack . extract $ mid)

record :: Connection -> MigrationId -> IO ()
record conn mid = void $ execute conn "INSERT INTO MIGRATION_INFO VALUES (?)" (Only mid)

runall :: Connection -> (Migration -> Ddl) -> [Migration] -> MigrationResultT IO [MigrationId]
runall c f ms =
  liftIO (begin c) >>
    foldM (\rs m ->
             EitherT $
               do e <- runEitherT (saferun c f m)
                  case e of
                    Left emsg -> rollback c >> (return . Left $ Context (reverse rs) (migration m) emsg True)
                    Right r -> return . Right $ r:rs) [] ms >>= \result -> liftIO (commit c) >> return (reverse result)

saferun :: Connection -> (Migration -> Ddl) -> Migration -> EitherT Text IO MigrationId
saferun c f m = EitherT $ handle (\e -> return (Left (pack . show $ (e :: SomeException)))) (fmap Right $ run c f m)

run :: Connection -> (Migration -> Ddl) -> Migration -> IO MigrationId
run c f m = execute_ c (fromString . unpack $ f m) >> record c (migration m) >> return (migration m)
