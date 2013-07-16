module Database.Migrate.Migration.FileStandard (
  migration
, up
, down
) where

import           Database.Migrate.Migration
import qualified Database.Migrate.Migration.File as File
import           System.FilePath

migration :: MigrationId -> FilePath -> Migration
migration mid prefix =
  File.migration mid (prefix <.> "up.ddl") (prefix <.> "down.ddl")

up :: MigrationId -> FilePath -> Migration
up mid prefix =
  File.up mid (prefix <.> ".up.ddl")

down :: MigrationId -> FilePath -> Migration
down mid prefix =
  File.down mid (prefix <.> "down.ddl")
