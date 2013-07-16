{-# LANGUAGE OverloadedStrings #-}
module Database.Migrate.Migration.Directory (
  migration
) where

import           Control.Applicative ((<$>))
import           Data.List (isSuffixOf, intercalate, groupBy)
import           Data.Text (Text, pack)

import           Database.Migrate.Migration
import qualified Database.Migrate.Migration.FileInferred as File
import           System.FilePath
import           System.Directory

-- TODO support baselines as well as change files

migration :: FilePath -> IO (Either Text [Migration])
migration path =
  (mkmigration . filter isMigrationFile) <$> getDirectoryContents path

unsuffix :: FilePath -> FilePath
unsuffix =
  dropExtension . dropExtension

isMigrationFile :: FilePath -> Bool
isMigrationFile p =
  isSuffixOf ".up.sql" p || isSuffixOf ".down.sql" p

isUp :: FilePath -> Bool
isUp =
  isMigrationType "up"

isMigrationType :: String -> FilePath -> Bool
isMigrationType t p =
  (takeExtension . takeExtension $ p) == t

mkmigration :: [FilePath] -> Either Text [Migration]
mkmigration paths =
  mapM (\group -> case group of
    a : _ : [] -> Right . File.migration $ unsuffix a
    a : [] ->  Right $ (if isUp a then File.up else File.down) (unsuffix a)
    _ -> Left . pack $ "Duplicate prefix detected in: " ++ (intercalate "," group)
  ) (groupBy (\a b -> unsuffix a == unsuffix b) paths)
