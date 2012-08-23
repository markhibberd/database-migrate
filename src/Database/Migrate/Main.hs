module Database.Migrate.Main (defaultMain) where

import qualified Paths_database_migrate as Program (version)

import qualified Data.Text as T
import Data.Version (showVersion)

import Database.PostgreSQL.Simple

import System.Console.CmdArgs.Explicit
import System.Directory
import System.Exit

usage = [
    "usage: migrate [-v|--verbose] [-r|--dry-run] [-t|--test-run] [-a|--auto] [-s|--scripts DIR]"
  , "               [-h|--host HOSTNAME] [-p|--port PORTNUMBER] [-u|--user USER]"
  , "               [-P|--password PASSWORD] [-d|--db DATABASE] [-g|--postgres] [-m|--mysql] [VERSION]"
  , "       migrate -h|--help"
  , "       migrate -V|--version"
  ]

data Arguments = Arguments {
    printhelp :: Bool
  , printversion :: Bool
  , dry :: Bool
  , test :: Bool
  , auto :: Bool
  , postgres :: Bool
  , mysql :: Bool
  , version :: Maybe String
  , scripts :: String
  } deriving (Eq, Show)

defaultArguments cwd = Arguments {
    printhelp = False
  , printversion = False
  , dry = False
  , test = False
  , auto = False
  , postgres = False
  , mysql = False
  , version = Nothing
  , scripts = cwd
  }

migratemode cwd =
  mode "migrate" (defaultArguments cwd) "" (flagArg (\v a -> Right $ a { version = Just v }) "VERSION") [
      flagNone [ "h", "help" ]     (\a -> a { printhelp = True })    ""
    , flagNone [ "V", "version" ]  (\a -> a { printversion = True }) ""
    , flagNone [ "r", "dry-run" ]  (\a -> a { dry = True })          ""
    , flagNone [ "g", "postgres" ]  (\a -> a { postgres = True })    ""
    , flagNone [ "m", "mysql" ]  (\a -> a { postgres = True })    ""
    ]

defaultMain =
  getCurrentDirectory >>= \cwd -> processArgs (migratemode cwd) >>= run

run args
  | printhelp args      = mapM_ putStrLn usage
  | printversion args   = putStrLn $ "migrate " ++ showVersion Program.version
  | otherwise           = migrate args

bomb msg =
  putStrLn msg >> exitFailure

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

