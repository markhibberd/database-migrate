module Database.Migrate.PostgreSQL where

import Database.PostgreSQL.Simple
import Database.Migrate.Core
import Data.Text
import Data.String (IsString(..))

instance MigrateDatabase IO Connection where
  runMigrations c f ms = fmap (const MigrationOk) $ withTransaction c (mapM_ (\m -> execute_ c (queryOf f m)) ms)

  getMigrations c = undefined

queryOf :: (Migration -> Change) -> Migration -> Query
queryOf f m = undefined

fromText :: Text -> Query
fromText = fromString . unpack
