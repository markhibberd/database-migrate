{-#LANGUAGE OverloadedStrings, ScopedTypeVariables#-}
module Database.Migrate.Core where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Maybe

import qualified Data.Set as S
import Data.List (sort)
import Data.Text hiding (foldr, filter, reverse, length)

import System.FilePath
import System.Directory
import System.IO

data MigrationId =
  MigrationId { extract :: Text } deriving (Eq, Show)

instance Ord MigrationId where
  compare a b =
   case (reads . unpack . extract $ a, reads . unpack . extract $ b) of
     ([(i :: Int, "")], [(j :: Int, "")]) -> compare i j
     _ -> compare (extract a) (extract b)

type Ddl = Text

data Migration =
  Migration {
      migration :: MigrationId
    , up :: Text
    , down :: Text
    , upsource :: Maybe FilePath
    , downsource :: Maybe FilePath
    } deriving (Eq, Show)

instance Ord Migration where
  compare a b = compare (migration a) (migration b)

data Context = Context {
    succeeded :: [MigrationId]
  , failed :: MigrationId
  , msg :: Text
  , rolledback :: Bool
  } deriving (Eq, Show)

type MigrationResultT = EitherT Context

class Monad m => MigrateDatabase m c where
  testconn :: MM c m Bool

  initialize :: MM c m ()
  initialized :: MM c m Bool

  runMigration :: Migration -> MM c m ()
  getMigrations :: MM c m [MigrationId]

  tx :: MM c m () -> MM c m Bool
  tx m = m >> return True

type Logger = String -> IO ()

data MigrateContext a = MigrateContext {
    ctxScripts :: FilePath
  , ctxConnection :: a
  , ctxDry :: Bool
  , ctxVerbose :: Bool
  }

data MigrateLog =
    MigrateApplied MigrationId
  | MigrationFailed MigrationId String
  | MigrationRolledback MigrationId String

newtype MM c m a =
  MM { runM :: ReaderT (MigrateContext c) (WriterT [MigrateLog] (MaybeT m)) a }

connection :: (Functor m, Monad m) => MM c m c
connection = MM $ fmap ctxConnection ask

scripts :: (Functor m, Monad m) => MM c m FilePath
scripts = MM $ fmap ctxScripts ask

isDryRun :: (Functor m, Monad m) => MM c m Bool
isDryRun= MM $ fmap ctxDry ask

isVerbose :: (Functor m, Monad m) => MM c m Bool
isVerbose = MM $ fmap ctxDry ask

mlog :: Monad m => MigrateLog -> MM c m ()
mlog l = MM . lift $ tell [l]

instance Monad f => Functor (MM c f) where
  fmap f a = a >>= \a' -> return (f a')

instance Monad m => Monad (MM c m) where
  return a = MM $ return a
  m >>= k  = MM $ do
    a <- runM m
    runM (k a)

instance MonadTrans (MM c) where
  lift = MM . lift . lift . lift

instance MonadIO m => MonadIO (MM c m) where
  liftIO = lift . liftIO

pick :: [Migration] -> [MigrationId] -> [Migration]
pick ms ids =
  let available = foldr (S.insert . migration) S.empty ms
      installed = S.fromList ids
      torun = S.difference available installed
   in filter (\m -> S.member (migration m) torun) ms

{--
latest :: MigrateDatabase m c => c -> [Migration] -> MigrationResultT m [MigrationId]
latest c migrations =
  do installed << getMigrations
  lift (getMigrations c) >>= \installed ->
    let toinstall = pick migrations installed

    runMigrations c up (pick migrations installed)
--}

find :: FilePath -> EitherT String IO [Migration]
find b = liftIO (getDirectoryContents b) >>= \fs -> liftM sort (liftIO (migrationids b fs) >>=
  mapM (\p ->
         do downexists <- liftIO $ doesFileExist (b </> p <.> "down.sql")
            unless downexists (left $ "no down.sql for migration [" ++ p ++ "]")
            u <- liftIO . readFile $ b </> p <.> "up.sql"
            d <- liftIO . readFile $ b </> p <.> "down.sql"
            right (Migration (MigrationId . pack $ p) (pack u) (pack d) (Just $ b </> p <.> "up.sql") (Just $ b </> p <.> "down.sql"))))

migrationids :: FilePath -> [FilePath] -> IO [String]
migrationids b ps =
  filterM (\p -> doesFileExist (b </> p)) ps >>= \files ->
    ((return . fmap dropExtensions)
      (filter (\p -> takeExtensions p == ".up.sql") files))

readFile' :: FilePath -> IO String
readFile' p = withFile p ReadMode hGetContents

hGetContents' :: Handle -> IO String
hGetContents' h = hGetContents h >>= \s -> length s `seq` return s

