{-# LANGUAGE OverloadedStrings #-}
module Database.Migrate.Migration.FileInferred (
  migration
, up
, down
, directory
) where

import           Data.List (isSuffixOf, intercalate, groupBy)
import           Data.Text (Text, pack)

import           Database.Migrate.Migration
import qualified Database.Migrate.Migration.File         as File
import qualified Database.Migrate.Migration.FileStandard as FileStandard
import           System.FilePath
import           System.Directory

directory :: FilePath -> IO (Either Text [Migration])
directory path = do
  changes <- (filter (\p -> isSuffixOf ".up.sql" p || isSuffixOf ".down.sql" p)) `fmap` getDirectoryContents path
  return $ mapM (\group ->
    case group of
      a : _ : [] -> (Right $ migration (dropExtension . dropExtension $ a))
      a : [] -> (Right $ (if ((takeExtension . takeExtension $ a) == "up") then File.up else File.down) (infer a) a)
      _ -> (Left . pack $ "Duplicate prefix detected in: " ++ (intercalate "," group))
    ) $ groupBy (\a b -> (dropExtension . dropExtension $ a) == (dropExtension . dropExtension $ b)) changes

migration :: FilePath -> Migration
migration prefix =
  FileStandard.migration (infer prefix) prefix

up :: FilePath -> Migration
up prefix =
  FileStandard.up (infer prefix) prefix

down :: FilePath -> Migration
down prefix =
  FileStandard.down (infer prefix) prefix

infer :: FilePath -> MigrationId
infer =
  pack . dropExtensions . last . splitPath
