{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Database.Migrate.Main (defaultMain) where

--import qualified Paths_database_migrate as Program (version)

import qualified Data.Text as T
import Data.Version (showVersion)

import Database.PostgreSQL.Simple

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
    flagReq  [ "H", "hostname" ]   (\a v -> a { info = (info a) { hostname = Just . T.pack $ v } }) e e
  , flagReq  [ "P", "password" ]   (\a v -> a { info = (info a) { password = Just . T.pack $ v } }) e e
  , flagReq  [ "p", "port" ]       (\a v -> a { info = (info a) { port = Just . T.pack $ v } }) e e
  , flagReq  [ "u", "user" ]       (\a v -> a { info = (info a) { user = Just . T.pack $ v } }) e e
  , flagReq  [ "D", "db" ]         (\a v -> a { info = (info a) { dbname = Just . T.pack $ v } }) e e
  , flagNone [ "g", "postgres" ]   (\a -> a { info = (info a) { conntype = Just PostgresConnType  } }) e e
  , flagNone [ "m", "mysql" ]      (\a -> a { info = (info a) { dbname = Just MysqlConnType } }) e e
  , flagNone [ "v", "verbose" ]    (\a -> a { verbose = True }) e e
  , flagNone [ "d", "dry-run" ]    (\a -> a { dry = True }) e e
  , flagReq  [ "s", "scripts" ]    (\a v -> a { scripts = Just . T.pack $ v }) e e
  ]

versionflag = (flagArg (\v a -> Right $ a { version = Just v }) "VERSION")

cmdmodes :: T.Text -> Arguments -> Mode Arguments
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

data MigrateConnectInfo = MigrateConnectInfo {
    hostname :: Maybe T.Text
  , password :: Maybe T.Text
  , port :: Maybe Int
  , user :: Maybe T.Text
  , dbname :: Maybe T.Text
  , conntype :: ConnType
  }

data DbMode =
    HelpMode
  | VersionMode
  | MigrateMode
  | UpMode
  | DownMode
  | ApplyMode
  | TestMode
  | InfoMode

data Arguments = Arguments {
    dbmode :: DbMode
  , dry :: Bool
  , verbose :: Bool
  , scripts :: T.Text
  , info :: MigrateConnectInfo
  , version :: Maybe T.Text
  } deriving (Eq, Show)

defaultArguments cwd = Arguments {
    dbmode = HelpMode
  , dry = False
  , verbose = False
  , scripts = T.pack $ cwd </> "migrations"
  , info = MigrateConnectInfo Nothing Nothing Nothing Nothing Nothing PostgresConnType
  , version = Nothing
  }

defaultMain cmd =
  getCurrentDirectory >>= \cwd -> processArgs (cmdmodes cmd cwd) >>= run

run args =
  case dbmode args of
    HelpMode -> mapM_ putStrLn usage
    VersionMode -> putStrLn $ "migrate " ++ "showVersion Program.version" -- FIX
    MigrateMode -> undefined
    UpMode -> undefined
    DownMode -> undefined
    ApplyMode -> undefined
    TestMode -> undefined
    InfoMode -> undefined

bomb msg =
  putStrLn msg >> exitFailure

buildconnnection connectinfo =
  case conntype connectinfo of
    PostgresConnType -> undefined
    MysqlConnType ->  undefined

migrate args =
  case (postgres args, mysql args) of
    (False, False) -> bomb "Must specify exactly one of -p or -m for database selection, specified none."
    (True, True) -> bomb "Must specify exactly one of -p or -m for database selection, specified two."
    (True, _) -> runpostgres args
    (_, True) -> runmysql args

runmysql _  =
  bomb "Not implemented yet."

runpostgres args =
  undefined

