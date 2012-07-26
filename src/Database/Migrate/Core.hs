module Database.Migrate.Core where

import Data.ByteString hiding (foldr, filter, reverse)
import qualified Data.Set as S
import Data.Text hiding (foldr, filter, reverse)
import Data.Time

newtype MigrationId =
  MigrationId String deriving (Eq, Show, Ord)

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
  | DbBoolean (Maybe Bool)
  | DbAutoInc

data Column = Column {
    name :: ColumnName
  , datatype :: DbType
  , nullable :: Bool
  , primary :: Bool
  }

data Change =
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
      migrationId :: MigrationId
    , up :: Change
    , down :: Change
    }

data MigrationResult =
  MigrationOk
  | MigrationRollback
  | MigrationFailed
  | MigrationUnsupported

class Monad m => MigrateDatabase m c where
  runMigrations :: c -> (Migration -> Change) -> [Migration] -> m MigrationResult
  getMigrations :: c -> m [MigrationId]

pick :: [Migration] -> [MigrationId] -> [Migration]
pick ms ids =
  let available = foldr (S.insert . migrationId) S.empty ms
      installed = S.fromList ids
      torun = S.difference available installed
   in filter (\m -> S.member (migrationId m) torun) ms

latest :: MigrateDatabase m c => c -> [Migration] -> m MigrationResult
latest c migrations =
  getMigrations c >>= \installed -> runMigrations c up (pick migrations installed)

