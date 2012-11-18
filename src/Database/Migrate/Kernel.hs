{-# LANGUAGE RankNTypes #-}
module Database.Migrate.Kernel where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Maybe

import Data.Text hiding (foldr, filter, reverse, length)

data ConnectionInfo m c =
  ConnectionInfo { getConnectionInfo :: m c }

data MigrationId =
  MigrationId Text

data MigrationLog =
    MigrationApplied Migration
  | MigrationFailed Migration String
  | MigrationRolledback Migration String

data Migration =
  Migration MigrationId Text Text

data MigrationRecords =
    NotInitialized
  | Initialized [MigrationId]

-- FIX should not be a list, want to index and sort migrations
data Migrations =
  Migrations [Migration]

data Migrate c m a =
  Migrate { runMigrate :: ReaderT (Migrations, c) (WriterT [MigrationLog] (MaybeT m)) a }

-- FIX consider mandating history
-- FIX add reverse engineer to extract current schema
data MigrateDatabase m c =
  MigrateDatbase {
      execute :: forall a. ConnectionInfo m c -> Migrate c m a -> m ([MigrationLog], Maybe a)
    , current :: Migrate c m MigrationRecords
    , initialize :: Migrate c m ()
    }

instance Monad f => Functor (Migrate c f) where
  fmap f a = a >>= \a' -> return (f a')

instance Monad m => Monad (Migrate c m) where
  return a = Migrate $ return a
  m >>= f = Migrate $ do
    a <- runMigrate m
    runMigrate (f a)

migrate :: (Functor m, Monad m) => ConnectionInfo m c -> MigrateDatabase m c -> Migrations -> m ([MigrationLog], Bool)
migrate ci db ms =
  do c <- getConnectionInfo ci
     _ <- runMaybeT $ fmap fst (runWriterT $ runReaderT (runMigrate $ current db) (ms, c))
     return ([], True)
