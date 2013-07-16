{-# LANGUAGE OverloadedStrings, LiberalTypeSynonyms #-}
module Database.Migrate.PostgreSQL (
  psqlMain
) where


import Control.Lens                    ((^.), to)
import Control.Monad                   (void)
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

import Data.String (IsString(..))
import Data.Text                       hiding (length)
import Data.Time

import Database.PostgreSQL.Simple
import Database.Migrate.Migration

import System.IO

type DbName = Text
type Db a = ReaderT Connection IO a

psqlMain :: IO ()
psqlMain = putStrLn "hello"

history :: Query
history =
  "CREATE TABLE IF NOT EXISTS migration_history (db VARCHAR(50), migration VARCHAR(50), action CHARACTER VARYING(20), at BIGINT, sql TEXT)"
versions :: Query
versions =
  "CREATE TABLE IF NOT EXISTS migration_versions (db VARCHAR(50), migration VARCHAR(50) PRIMARY KEY)"

install :: Query
install =
  "INSERT INTO migration_versions (db, migration) VALUES (?, ?)"

uninstall :: Query
uninstall =
  "DELETE FROM migration_versions (db, migration) WHERE db = ? AND migration = ?"

addhistory :: Query
addhistory =
  "INSERT INTO migration_history (db, migration, action, at, sql) VALUES (?, ?, ?, ?, ?)"

up :: DbName -> Migration -> Db ()
up n =
  change n "up" _upChange

down :: DbName -> Migration -> Db ()
down n =
  change n "down" _downChange

change :: DbName -> Text -> (Migration -> Change) -> Migration -> Db ()
change n mode getChange m = do
  t <- now
  change <- liftIO . raw . getChange $ m
  run addhistory (n, m^.migrationId, mode, t, change)
  run install (n, m^.migrationId)
  run_ . mkquery $ change

run :: ToRow a => Query -> a -> Db ()
run q a = ask >>= \c -> liftIO . void $ execute c q a

run_ :: Query -> Db ()
run_ q = ask >>= \c -> liftIO . void $ execute_ c q

now :: Db UTCTime
now = liftIO getCurrentTime

mkquery :: Text -> Query
mkquery = fromString . unpack

raw :: Change -> IO Text
raw (Ddl q) = return q
raw (DdlFile q) = readFile q >>= return . pack
raw (Dud msg) = error . unpack $ msg -- FIX
raw NoOp = return "select true;"

readFile' :: FilePath -> IO String
readFile' p = withFile p ReadMode hGetContents'

hGetContents' :: Handle -> IO String
hGetContents' h = hGetContents h >>= \s -> length s `seq` return s
