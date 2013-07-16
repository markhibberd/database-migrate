{-# LANGUAGE OverloadedStrings #-}
module Database.Migrate.Migration.Ddl (
  migration
, up
, down
) where

import Database.Migrate.Migration
import Data.Text

migration :: MigrationId -> Text -> Text -> Migration
migration mid up' down' =
  Migration mid (Ddl up') (Ddl down')

up :: MigrationId -> Text -> Migration
up mid up' =
  Migration mid (Ddl up') NoOp

down :: MigrationId -> Text -> Migration
down mid down' =
  Migration mid NoOp (Ddl down')
