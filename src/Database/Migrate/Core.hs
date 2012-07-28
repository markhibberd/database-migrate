{-#LANGUAGE OverloadedStrings #-}
module Database.Migrate.Core where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

import qualified Data.Set as S
import Data.Text hiding (foldr, filter, reverse, length)

import System.FilePath
import System.Directory
import System.IO

type MigrationId = Text
type Ddl = Text

data Migration =
  Migration {
      migration :: MigrationId
    , up :: Text
    , down :: Text
    , upsource :: Maybe FilePath
    , downsource :: Maybe FilePath
    }

data Context = Context {
    succeeded :: [MigrationId]
  , failed :: MigrationId
  , msg :: Text
  , rolledback :: Bool
  } deriving (Eq, Show)

type MigrationResultT = EitherT Context

class Monad m => MigrateDatabase m c where
  initialize :: c -> m ()
  runMigrations :: c -> (Migration -> Ddl) -> [Migration] -> MigrationResultT m [MigrationId]
  getMigrations :: c -> m [MigrationId]

pick :: [Migration] -> [MigrationId] -> [Migration]
pick ms ids =
  let available = foldr (S.insert . migration) S.empty ms
      installed = S.fromList ids
      torun = S.difference available installed
   in filter (\m -> S.member (migration m) torun) ms

latest :: MigrateDatabase m c => c -> [Migration] -> MigrationResultT m [MigrationId]
latest c migrations =
  lift (getMigrations c) >>= \installed -> runMigrations c up (pick migrations installed)

find :: FilePath -> EitherT String IO [Migration]
find b = liftIO (getDirectoryContents b) >>= \fs -> liftIO (migrationids b fs) >>=
  mapM (\p ->
         do downexists <- liftIO $ doesFileExist (b </> p <.> "down.sql")
            unless downexists (left $ "no down.sql for migration [" ++ p ++ "]")
            u <- liftIO . readFile $ b </> p <.> "up.sql"
            d <- liftIO . readFile $ b </> p <.> "down.sql"
            right (Migration (pack p) (pack u) (pack d) (Just $ b </> p <.> "up.sql") (Just $ b </> p <.> "down.sql")))

migrationids :: FilePath -> [FilePath] -> IO [String]
migrationids b ps =
  filterM (\p -> doesFileExist (b </> p)) ps >>= \files ->
    return (filter (\p -> takeExtensions p == ".up.sql" ) files) >>= \ups -> return (fmap dropExtensions ups)

readFile' :: FilePath -> IO String
readFile' p = withFile p ReadMode hGetContents

hGetContents' :: Handle -> IO String
hGetContents' h = hGetContents h >>= \s -> length s `seq` return s

