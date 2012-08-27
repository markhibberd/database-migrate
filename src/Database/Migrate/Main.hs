{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Database.Migrate.Main (defaultMain) where

import qualified Paths_database_migrate as Program (version)

import Control.Monad
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Maybe

import qualified Data.Text as T
import Data.Maybe
import Data.Version (showVersion)

import Database.Migrate.Core
import Database.Migrate.PostgreSQL ()

import qualified Database.PostgreSQL.Simple as PG

import System.Console.CmdArgs.Explicit
import System.Directory
import System.FilePath
import System.Exit

ignore :: Arg Arguments
ignore = flagArg (\_ a -> Right a) ""

e :: String
e = ""

usage = [
    "usage: db [-v|--verbose] [-d|--dry-run] [-t|--test-run] [-a|--auto] [-s|--scripts DIR]"
  , "               [-h|--host HOSTNAME] [-p|--port PORTNUMBER] [-u|--user USER]"
  , "               [-P|--password PASSWORD] [-D|--db DATABASE] [-g|--postgres] [-m|--mysql] [VERSION]"
  , "       db -h|--help"
  , "       db -V|--version"
  ]


globalflags :: [Flag Arguments]
globalflags = [
    flagNone [ "h", "help" ]     (\a -> a { dbmode = HelpMode })    e
  , flagNone [ "V", "version" ]  (\a -> a { dbmode = VersionMode }) e
  ]

connectflags :: [Flag Arguments]
connectflags = [
    flagReq  [ "H", "hostname" ]   (\v a -> Right $ a { info = (info a) { hostname = Just v } }) e e
  , flagReq  [ "P", "password" ]   (\v a -> Right $ a { info = (info a) { password = Just v } }) e e
  , flagReq  [ "p", "port" ]       (\v a ->
                                     case reads v of
                                       [(i, "")] -> Right $ a { info = (info a) { port = Just i } }
                                       _ -> Left "invalid port number"
                                   ) e e

  , flagReq  [ "u", "user" ]       (\v a -> Right $ a { info = (info a) { user = Just v } }) e e
  , flagReq  [ "D", "db" ]         (\v a -> Right $ a { info = (info a) { dbname = Just v } }) e e
  , flagNone [ "g", "postgres" ]   (\a -> a { info = (info a) { conntype = PostgresConnType  } }) e
  , flagNone [ "m", "mysql" ]      (\a -> a { info = (info a) { conntype = MysqlConnType } }) e
  , flagNone [ "v", "verbose" ]    (\a -> a { verbose = True }) e
  , flagNone [ "d", "dry-run" ]    (\a -> a { dry = True }) e
  , flagReq  [ "s", "scripts" ]    (\v a -> Right $ a { scripts = v }) e e
  ]

versionflag = (flagArg (\v a -> Right $ a { version = Just v }) "VERSION")

cmdmodes :: String -> Arguments -> Mode Arguments
cmdmodes cmd initial =
  modes cmd initial "" [
      mode "migrate" (initial { dbmode = MigrateMode }) "" ignore connectflags
    , mode "up" (initial { dbmode = UpMode }) "" versionflag connectflags
    , mode "down" (initial { dbmode = DownMode }) "" versionflag connectflags
    , mode "apply" (initial { dbmode = ApplyMode }) "" versionflag connectflags
    , mode "test" (initial { dbmode = TestMode }) "" versionflag connectflags
    , mode "info" (initial { dbmode = InfoMode }) "" ignore connectflags
    , mode "help" (initial { dbmode = HelpMode }) "" ignore []
    , mode "version" (initial { dbmode = VersionMode }) "" ignore []
    ]

data ConnType =
    PostgresConnType
  | MysqlConnType
  deriving (Eq, Show)

data MigrateConnectInfo = MigrateConnectInfo {
    hostname :: Maybe String
  , password :: Maybe String
  , port :: Maybe Int
  , user :: Maybe String
  , dbname :: Maybe String
  , conntype :: ConnType
  } deriving (Eq, Show)

data DbMode =
    HelpMode
  | VersionMode
  | MigrateMode
  | UpMode
  | DownMode
  | ApplyMode
  | TestMode
  | InfoMode
  deriving (Eq, Show)

data Arguments = Arguments {
    dbmode :: DbMode
  , dry :: Bool
  , verbose :: Bool
  , scripts :: String
  , info :: MigrateConnectInfo
  , version :: Maybe String
  } deriving (Eq, Show)

defaultArguments cwd = Arguments {
    dbmode = HelpMode
  , dry = False
  , verbose = False
  , scripts = cwd </> "migrations"
  , info = MigrateConnectInfo Nothing Nothing Nothing Nothing Nothing PostgresConnType
  , version = Nothing
  }

defaultMain cmd =
  getCurrentDirectory >>= \cwd -> processArgs ((cmdmodes cmd (defaultArguments cwd)) {modeGroupFlags = toGroup $ globalflags} )>>= run putStrLn

run logger args =
  case dbmode args of
    HelpMode -> mapM_ putStrLn usage
    VersionMode -> putStrLn $ "migrate " ++ showVersion Program.version
    MigrateMode -> buildconnnection (info args)  >>= migratemode logger (scripts args)
    UpMode -> buildconnnection (info args) >>= upmode
    DownMode -> buildconnnection (info args) >>= downmode
    ApplyMode -> buildconnnection (info args) >>= applymode
    TestMode -> buildconnnection (info args) >>= testmode
    InfoMode -> buildconnnection (info args) >>= infomode

bomb failwith =
  putStrLn failwith >> exitFailure

buildconnnection ci =
  case conntype ci of
    PostgresConnType ->
      PG.connect (PG.defaultConnectInfo {
          PG.connectHost = fromMaybe (PG.connectHost PG.defaultConnectInfo) (hostname ci)
        , PG.connectPort = maybe (PG.connectPort PG.defaultConnectInfo) (fromIntegral)  (port ci)
        , PG.connectUser = fromMaybe (PG.connectUser PG.defaultConnectInfo) (user ci)
        , PG.connectPassword = fromMaybe (PG.connectPassword PG.defaultConnectInfo) (password ci)
        , PG.connectDatabase = fromMaybe (PG.connectDatabase PG.defaultConnectInfo) (dbname ci)
        }) >>= \conn -> testconn conn >>= \ok -> if ok then return conn else bomb ("could not connect to database with: " ++ show ci)
    MysqlConnType -> error "not implemented"



migratemode :: MigrateDatabase IO a => M a ()
migratemode = undefined

upmode :: MigrateDatabase IO a => a -> IO ()
upmode connection = undefined

downmode :: MigrateDatabase IO a => a -> IO ()
downmode connection = undefined

applymode :: MigrateDatabase IO a => a -> IO ()
applymode connection = undefined

testmode :: MigrateDatabase IO a => a -> IO ()
testmode connection = undefined

infomode :: MigrateDatabase IO a => a -> IO ()
infomode connection = undefined

