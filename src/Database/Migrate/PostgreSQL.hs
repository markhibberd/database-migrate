{-#LANGUAGE OverloadedStrings #-}
module Database.Migrate.PostgreSQL where

import Control.Exception (SomeException(..), handle)
import Database.PostgreSQL.Simple
import Database.Migrate.Core
import Data.Text hiding (filter)
import Data.Monoid
import Data.String (IsString(..))

(<<>>) :: Text -> Text -> Text
(<<>>) = mappend

instance MigrateDatabase IO Connection where
  initialize c = fmap (const ()) $ execute_ c "CREATE TABLE IF NOT EXISTS MIGRATION_INFO (MIGRATION VARCHAR(50) PRIMARY KEY)"
  runMigrations c f ms =
    handle (\e -> return . MigrationRollback . pack . show $ (e :: SomeException))
           (withTransaction c (mapM_ (queryOf c f) ms >> return MigrationOk))
  getMigrations c =
    do ms <- query_ c "SELECT MIGRATION FROM MIGRATION_INFO"
       return $ fmap (MigrationId . fromOnly) ms

record :: Connection -> MigrationId -> IO ()
record conn (MigrationId mid) =
  fmap (const ()) $ execute conn "INSERT INTO MIGRATION_INFO VALUES (?)" (Only mid)

queryOf :: Connection -> (Migration -> Change) -> Migration -> IO ()
queryOf c f m = queryOf' c (f m) >> (record c (migrationId m))

queryOf' :: Connection -> Change -> IO ()
queryOf' conn ch = fmap (const ()) $
  case ch of
    CreateTable t cs ->
      let q = "CREATE TABLE ? (" <<>> (intercalate "," (fmap col cs)) <<>> ")"
       in execute conn (fromText q) (Only t)
    DropTable t -> execute conn "DROP TABLE ?" (Only t)
    RenameTable t t' -> execute conn "ALTER TABLE ? RENAME TO ?" (t, t')
    AddColumn t c -> execute conn (fromText $ "ALTER TABLE ? ADD COLUMN " <<>> col c) (Only t)
    DropColumn t c -> execute conn "ALTER TABLE ? DROP COLUMN ?" (t, c)
    RenameColumn t c c' -> execute conn "ALTER TABLE ? RENAME COLUMN ? TO ?" (t, c, c')
    UpdateColumn t c -> undefined
    AddIndex t i cs -> undefined
    DropIndex _ i -> execute conn "DROP INDEX ?" (Only i)
    RunSql s -> execute_ conn (fromText s)

fromText :: Text -> Query
fromText = fromString . unpack

col :: Column -> Text
col (Column n (DbString l d) nul _) =
  n <<>> " VARCHAR(" <<>> (pack . show $ l) <<>> ")" <<>>
  dfault (\t -> "\"" <<>> t <<>> "\"") d <<>>
  notnull nul
col (Column n (DbText d) nul _) =
  n <<>> " TEXT" <<>>
  dfault (\t -> "\"" <<>> t <<>> "\"") d <<>>
  notnull nul
col (Column n (DbSmallInt d) nul _) =
  n <<>> " SMALLINT" <<>>
  dfault (pack . show) d <<>>
  notnull nul
col (Column n (DbInt d) nul _) = undefined
  n <<>> " INTEGER" <<>>
  dfault (pack . show) d <<>>
  notnull nul
col (Column n (DbBigInt d) nul _) = undefined
  n <<>> " BIGINT" <<>>
  dfault (pack . show) d <<>>
  notnull nul
col (Column n (DbNumeric p d) nul _) =
  n <<>> " NUMERIC" <<>> maybe "" (\i -> "(" <<>> (pack . show $ i) <<>> ")") p <<>>
  dfault (pack . show) d <<>>
  notnull nul
col (Column n (DbNumericWithScale p s d) nul _) =
  n <<>> " NUMERIC" <<>>
    (case (p, s) of
      (Nothing, Nothing) -> ""
      (Just p', Nothing) -> "(" <<>> (pack . show $ p') <<>> ")"
      (Just p', Just s') -> "(" <<>> (pack . show $ p') <<>> "," <<>> (pack . show $ s') <<>> ")"
      (Nothing, Just s') -> "(0," <<>> (pack . show $ s') <<>> ")") <<>>
  dfault (\x -> (pack . show . fst $ x) <<>> "." <<>> (pack . show . snd $ x)) d <<>>
  notnull nul
col (Column n (DbFloat d) nul _) = undefined
  n <<>> " FLOAT" <<>>
  dfault (pack . show) d <<>>
  notnull nul
col (Column n (DbDouble d) nul _) = undefined
  n <<>> " DOUBLE PRECISION" <<>>
  dfault (pack . show) d <<>>
  notnull nul
col (Column n (DbTimestamp d) nul _) =
  n <<>> " TIMESTAMP" <<>>
  dfault (pack . show) d <<>>
  notnull nul
col (Column n (DbDate d) nul _) =
  n <<>> " DATE" <<>>
  dfault (pack . show) d <<>>
  notnull nul
col (Column n (DbTime d) nul _) =
  n <<>> " TIME" <<>>
  dfault (pack . show) d <<>>
  notnull nul
col (Column n DbBinary nul _) =
  n <<>> notnull nul
col (Column n (DbBoolean d) nul _) =
  n <<>> " BOOLEAN" <<>>
  dfault (\b -> if b then "TRUE" else "FALSE") d <<>>
  notnull nul
col (Column n DbAutoInc nul _) =
  n <<>> " SERIAL" <<>>
  notnull nul

dfault :: (a -> Text) -> Default a -> Text
dfault _ DefaultNone = ""
dfault f (DefaultValue a) = " DEFAULT " <<>> f a
dfault _ (DefaultFunc t) = " DEFAULT " <<>> t

notnull :: Bool -> Text
notnull nul = if nul then "" else " NOT NULL"

pk :: [Column] -> Text
pk cs =
  let primaries = filter primary cs
   in "PRIMARY KEY (" <<>> intercalate "," (fmap name primaries) <<>> ")"
