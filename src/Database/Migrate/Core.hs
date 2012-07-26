module Database.Migrate.Core where

import Data.Text

data Column = Column
data Constraint = Constraint
data Index = Index
data Table = Table Text [Column] [Constraint] [Index]

newtype MigrationId =
  MigrationId String

type TableName = Text
type IndexName = Text
type ColumnName = Text

data DbType =
    DbString (Maybe Int) (Maybe Text)
  | DbText (Maybe Int) (Maybe Text)
  | DbInteger (Maybe Int) (Maybe Int)
  | DbDouble (Maybe Double)
  | DbTimestamp (Maybe UTCTime)
  | DbDate (Maybe Day)
  | DbTime (Maybe TimeOfDay)
  | DbBinary (Maybe Int) (Maybe ByteString)
  | DbBoolean (Maybe Boolean)
  | DbAutoInc

data Column = Column {
    name :: ColumnName
  , datatype :: DbType
  , nullable :: Bool
  , primary :: Bool
  }

data Update =
  CreateTable TableName [Column]
  | DropTable TableName
  | RenameTable TableName TableName
  | AddColumn TableName Column
  | DropColumn TableName ColumnName
  | RenameColumn TableName ColumnName ColumnName
  | UpdateColumn TableName Column
  | AddIndex TableName IndexName [ColumnName]
  | DropIndex TableName IndexName
  | RunSql Text

data Migration =
  Migration {
    , migrationId :: MigrationId
    , up :: Update
    , down :: Update
    }

data MigrationResult =
  MigrationOk
  | MigrationRollback
  | MigrationFailed
  | MigrationUnsupported

class MigrateDatabase m c where
  runMigrations :: c -> [Migration] -> m MigrationResult
  getMigrations :: c -> m [MigrationId]






add ::
  Int
  -> Int
  -> Int
add =
  (+)
