{-# LANGUAGE OverloadedStrings, LiberalTypeSynonyms #-}
module Database.Migrate.PostgreSQL (
  psqlMain
, migrate
) where

import           Control.Lens                    ((^.))
import           Control.Monad                   (void, forM_)
import           Control.Monad.Trans.Reader
import           Control.Monad.IO.Class

import qualified Data.Set as S
import           Data.String (IsString(..))
import           Data.Text                       hiding (length, foldr, filter)
import           Data.Time

import           Database.PostgreSQL.Simple
import           Database.Migrate.Migration

import           System.IO
import           System.FilePath


data DbRead = DbRead { getConnection :: Connection, getDbName :: DbName,  getBase :: FilePath }
type DbName = Text
type Db a = ReaderT DbRead IO a

psqlMain :: IO ()
psqlMain = putStrLn "hello"

migrate :: DbName -> Connection -> FilePath -> [Migration] -> IO ()
migrate n c b migrations =
  runReaderT (run_ history >> run_ versions >> migrate' migrations) (DbRead c n b)

migrate' :: [Migration] -> Db ()
migrate' migrations = do

  installed <- current
  let nu = missing migrations installed
  forM_ nu $ \m -> do
    up m
    liftIO . putStrLn $ "Installed <" ++ (unpack . _migrationId $ m) ++ ">"

history :: Query
history =
  "CREATE TABLE IF NOT EXISTS migration_history (db VARCHAR(50), migration VARCHAR(50), action CHARACTER VARYING(20), at TIMESTAMPTZ, sql TEXT)"

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

currentversions :: Query
currentversions =
  "SELECT migration FROM migration_versions WHERE db = ?"

current :: Db [MigrationId]
current = do
  n <- ask >>= return . getDbName
  unonly $ list currentversions (Only n)

up :: Migration -> Db ()
up =
  change "up" _upChange

down :: Migration -> Db ()
down =
  change "down" _downChange

change :: Text -> (Migration -> Change) -> Migration -> Db ()
change mode getChange m = do
  t <- now
  n <- ask >>= return . getDbName
  ch <- raw . getChange $ m
  run addhistory (n, m^.migrationId, mode, t, ch)
  run install (n, m^.migrationId)
  run_ . mkquery $ ch

run :: ToRow a => Query -> a -> Db ()
run q a = do
  c <- ask >>= return . getConnection
  liftIO . void $ execute c q a

run_ :: Query -> Db ()
run_ q = do
  c <- ask >>= return . getConnection
  liftIO . void $ execute_ c q

list :: (ToRow a, FromRow b) => Query -> a -> Db [b]
list q a = do
  c <- ask >>= return . getConnection
  liftIO $ query c q a

now :: Db UTCTime
now = liftIO getCurrentTime

mkquery :: Text -> Query
mkquery = fromString . unpack

raw :: Change -> Db Text
raw (Ddl q) = return q
raw (Dud msg) = error . unpack $ msg -- FIX
raw NoOp = return "select true;"
raw (DdlFile q) = do
  b <- ask >>= return . getBase
  liftIO $ readFile' (b </> q) >>= return . pack

readFile' :: FilePath -> IO String
readFile' p = withFile p ReadMode hGetContents'

hGetContents' :: Handle -> IO String
hGetContents' h = hGetContents h >>= \s -> length s `seq` return s

unonly :: Db [Only a] -> Db [a]
unonly = fmap (fmap (\(Only a) -> a))

missing :: [Migration] -> [MigrationId] -> [Migration]
missing migrations applied =
  let available = foldr (S.insert . _migrationId) S.empty migrations
      installed = S.fromList applied
      torun = S.difference available installed
   in filter (\m -> S.member (_migrationId m) torun) migrations
