{-#LANGUAGE OverloadedStrings #-}
module Database.Migrate.Core where

import qualified Data.Set as S
import Data.Text hiding (foldr, filter, reverse)
import Data.Time

newtype MigrationId =
  MigrationId Text deriving (Eq, Show, Ord)


type TableName = Text
type IndexName = Text
type ColumnName = Text

data Default a =
    DefaultNone
  | DefaultValue a
  | DefaultFunc Text

foldDefault :: b -> (a -> b) -> (Text -> b) -> Default a -> b
foldDefault b _ _ DefaultNone = b
foldDefault _ f _ (DefaultValue a) = f a
foldDefault _ _ f (DefaultFunc t) = f t

data DbType =
    DbString Int (Default Text)
  | DbText (Default Text)
  | DbSmallInt (Default Int)
  | DbInt (Default Int)
  | DbBigInt (Default Integer)
  | DbNumeric (Maybe Int) (Default Integer)
  | DbNumericWithScale (Maybe Int) (Maybe Int) (Default (Integer, Integer))
  | DbFloat (Default Float)
  | DbDouble (Default Double)
  | DbTimestamp (Default UTCTime)
  | DbDate (Default Day)
  | DbTime (Default TimeOfDay)
  | DbBinary
  | DbBoolean (Default Bool)
  | DbAutoInc

data Column = Column {
    name :: Text
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

data ChangeSet = ChangeSet {
    up :: Change
  , down :: Change
  }

data Migration =
  Migration {
      migrationId :: MigrationId
    , changes :: [ChangeSet]
    }


createTable :: TableName -> [Column] -> ChangeSet
createTable t cs = ChangeSet (CreateTable t cs) (DropTable t)

surrogate :: Column
surrogate = Column "id" DbAutoInc  False True

stdstring :: ColumnName -> Column
stdstring n = Column n (DbString 255 DefaultNone) True False

data MigrationResult =
  MigrationOk
  | MigrationRollback Text
  | MigrationFailed Text
  | MigrationUnsupported
  deriving (Eq, Show)

class Monad m => MigrateDatabase m c where
  initialize :: c -> m ()
  runMigrations :: c -> (Migration -> [Change]) -> [Migration] -> m MigrationResult
  getMigrations :: c -> m [MigrationId]

pick :: [Migration] -> [MigrationId] -> [Migration]
pick ms ids =
  let available = foldr (S.insert . migrationId) S.empty ms
      installed = S.fromList ids
      torun = S.difference available installed
   in filter (\m -> S.member (migrationId m) torun) ms

latest :: MigrateDatabase m c => c -> [Migration] -> m MigrationResult
latest c migrations =
  getMigrations c >>= \installed -> runMigrations c (\m -> fmap up $ changes m) (pick migrations installed)

