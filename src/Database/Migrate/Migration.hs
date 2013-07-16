{-# LANGUAGE TemplateHaskell, LiberalTypeSynonyms #-}
module Database.Migrate.Migration where

import Data.Text
import Control.Lens

type MigrationId = Text

data Change =
    Ddl Text
  | DdlFile FilePath
  | Dud Text
  | NoOp
  deriving (Show, Eq)
-- TODO | AddTable Text TableDef
-- TODO | AddColumn Text Text ColumnDef
-- TODO | DropTable Text
-- TODO | DropColumn Text
-- TODO | AlterColumn Text Text ColumnDef


data Migration =
    Migration { _migrationId :: MigrationId, _upChange :: Change, _downChange :: Change }

makeLenses ''Migration
