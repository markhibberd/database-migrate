{-#LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Database.Migrate.Kernel where

import Database.Migrate.Data

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer

import qualified Data.Set as S
import Data.Functor

mlog :: Monad m => MigrationLog -> Migrate c m ()
mlog l = Migrate . lift . lift $ tell [l]

connection :: (Functor m, Monad m) => Migrate c m c
connection = Migrate $ fmap snd ask

store :: (Functor m, Monad m) => Migrate c m Migrations
store = Migrate $ fmap fst ask

dryrun :: Monad m => MigrateDatabase m c -> MigrateDatabase m c
dryrun db =
  MigrateDatabase {
      current = current db
    , initialize = return ()
    , runSql = \_ -> return ()
    , recordInstall = \_ -> return ()
    , recordRollback = \_ -> return ()
    }

migrate :: (Functor m, Monad m) => MigrateDatabase m c -> Migrate c m ()
migrate db =
  do records <- current db
     ms <- store
     migrations <- case records of
       NotInitialized -> getMigrations ms <$ (mlog DatabaseInitialized >> initialize db)
       Initialized mids -> return $ missing ms mids
     forM_ migrations (\m -> mlog (MigrationApplied m) >> (runSql db (up m) >> recordInstall db m))

missing :: Migrations -> [MigrationId] -> [Migration]
missing ms applied =
  let migrations = getMigrations ms
      available = foldr (S.insert . migrationId) S.empty migrations
      installed = S.fromList applied
      torun = S.difference available installed
   in filter (\m -> S.member (migrationId m) torun) migrations

executeMigrate :: Monad m => Migrations -> c -> Migrate c m a -> m (Maybe a, [MigrationLog])
executeMigrate = \ms c m -> runWriterT (runMaybeT (runReaderT (runMigrate m) (ms, c)))
