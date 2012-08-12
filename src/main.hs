{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
import qualified Paths_database_migrate as Program (version)

import qualified Data.Text as T
import Data.Version (showVersion)

import Database.PostgreSQL.Simple

import System.Console.CmdArgs.Explicit
import System.Directory
import System.Exit

usage = [
    "usage: migrate [-v|--verbose] [-d|--dry-run] [-t|--test-run] [-a|--auto] [-s|--scripts DIR]"
  , "               [-p|--postgres CONNECTION] [-m|--mysql CONNECTION] [VERSION]"
  , "       migrate -h|--help"
  , "       migrate -V|--version"
  ]

data Arguments = Arguments {
    printhelp :: Bool
  , printversion :: Bool
  , dry :: Bool
  , test :: Bool
  , auto :: Bool
  , postgres :: Maybe String
  , mysql :: Maybe String
  , version :: Maybe String
  , scripts :: String
  } deriving (Eq, Show)

defaultArguments cwd = Arguments {
    printhelp = False
  , printversion = False
  , dry = False
  , test = False
  , auto = False
  , postgres = Nothing
  , mysql = Nothing
  , version = Nothing
  , scripts = cwd
  }

migratemode cwd =
  mode "migrate" (defaultArguments cwd) "" (flagArg (\v a -> Right $ a { version = Just v }) "VERSION") [
      flagNone [ "h", "help" ]     (\a -> a { printhelp = True })    "print help and exit"
    , flagNone [ "V", "version" ]  (\a -> a { printversion = True }) "print version and exit"
    , flagNone [ "d", "dry-run" ]  (\a -> a { dry = True })          "do not perform any updates"
    ]

main =
  getCurrentDirectory >>= \cwd -> processArgs (migratemode cwd) >>= run

run args
  | printhelp args      = mapM_ putStrLn usage
  | printversion args   = putStrLn $ "migrate " ++ showVersion Program.version
  | otherwise           = migrate args


bomb msg =
  putStrLn msg >> exitFailure

migrate args =
  case (postgres args, mysql args) of
    (Nothing, Nothing) -> bomb "Must specify at exactly one of -p or -m for database selection, specified none."
    (Just _, Just _) -> bomb "Must specify at exactly one of -p or -m for database selection, specified two."
    (Just p, _) -> runpostgres args p
    (_, Just m) -> runmysql args m


runmysql _ _ =
  bomb "Not implemented yet."

runpostgres = undefined


