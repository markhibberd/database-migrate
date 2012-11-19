module Database.Migrate.Loader where

import Database.Migrate.Data

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either

import Data.List (sort)
import Data.Text hiding (filter, length)

import System.FilePath
import System.Directory
import System.IO

find :: FilePath -> EitherT String IO Migrations
find b = liftIO (getDirectoryContents b) >>= \fs -> liftM sort (liftIO (migrationids b fs) >>=
  mapM (\p ->
         do downexists <- liftIO $ doesFileExist (b </> p <.> "down.sql")
            unless downexists (left $ "no down.sql for migration [" ++ p ++ "]")
            u <- liftIO . readFile $ b </> p <.> "up.sql"
            d <- liftIO . readFile $ b </> p <.> "down.sql"
            right (Migration (MigrationId . pack $ p) (pack u) (pack d) ))) >>= \ms -> return $ Migrations ms

migrationids :: FilePath -> [FilePath] -> IO [String]
migrationids b ps =
  filterM (\p -> doesFileExist (b </> p)) ps >>= \files ->
    ((return . fmap dropExtensions)
      (filter (\p -> takeExtensions p == ".up.sql") files))

readFile' :: FilePath -> IO String
readFile' p = withFile p ReadMode hGetContents

hGetContents' :: Handle -> IO String
hGetContents' h = hGetContents h >>= \s -> length s `seq` return s
