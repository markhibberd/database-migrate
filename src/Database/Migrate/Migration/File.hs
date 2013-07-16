module Database.Migrate.Migration.File (
  migration
, up
, down
) where

import Database.Migrate.Migration

migration :: MigrationId -> FilePath -> FilePath -> Migration
migration mid up' down' =
  Migration mid (DdlFile up') (DdlFile down')

up :: MigrationId -> FilePath -> Migration
up mid up' =
  Migration mid (DdlFile up') NoOp

down :: MigrationId -> FilePath -> Migration
down mid down' =
  Migration mid NoOp (DdlFile down')
