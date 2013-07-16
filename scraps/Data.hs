{-#LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Database.Migrate.Data where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Maybe

import Data.Text hiding (foldr, filter, reverse, length)

data MigrationId =
  MigrationId { extract :: Text } deriving (Eq, Show)

instance Ord MigrationId where
  compare a b =
   case (reads . unpack . extract $ a, reads . unpack . extract $ b) of
     ([(i :: Int, "")], [(j :: Int, "")]) -> compare i j
     _ -> compare (extract a) (extract b)

data MigrationLog =
    DatabaseInitialized
  | MigrationApplied Migration
  | MigrationFailed Migration String
  | MigrationRolledback Migration String
  deriving (Eq, Show)

data Migration =
  Migration {
      migrationId :: MigrationId
    , up :: Text
    , down :: Text
    } deriving (Eq, Show)

instance Ord Migration where
  compare a b = compare (migrationId a) (migrationId b)

data MigrationRecords =
    NotInitialized
  | Initialized [MigrationId]

-- FIX should not be a list, want to index and sort migrations
data Migrations =
  Migrations { getMigrations :: [Migration] }

data Migrate c m a =
  Migrate { runMigrate :: ReaderT (Migrations, c) (MaybeT (WriterT [MigrationLog] m)) a }

-- FIX consider adding history
-- FIX add reverse engineer to extract current schema
data MigrateDatabase m c =
  MigrateDatabase {
      current :: Migrate c m MigrationRecords
    , initialize :: Migrate c m ()
    , runSql :: Text -> Migrate c m ()
    , recordInstall :: Migration -> Migrate c m ()
    , recordRollback :: Migration -> Migrate c m ()
    }

instance Monad f => Functor (Migrate c f) where
  fmap f a = a >>= \a' -> return (f a')

instance Monad m => Monad (Migrate c m) where
  return a = Migrate $ return a
  m >>= f = Migrate $ do
    a <- runMigrate m
    runMigrate (f a)

instance MonadTrans (Migrate c) where
  lift = Migrate . lift . lift . lift

instance MonadIO m => MonadIO (Migrate c m) where
  liftIO = lift . liftIO
