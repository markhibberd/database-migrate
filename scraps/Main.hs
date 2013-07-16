{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Database.Migrate.Main (defaultMain, defaultMain') where

import qualified Paths_database_migrate as Program (version)

import Control.Monad
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Maybe

import qualified Data.Text as T
import Data.Maybe
--import Data.Version (showVersion)

import Database.Migrate.Data
import Database.Migrate.Kernel
import Database.Migrate.Loader
import Database.Migrate.PostgreSQL

import qualified Database.PostgreSQL.Simple as PG

import System.Console.CmdArgs.Explicit
import System.Directory
import System.FilePath
import System.Exit
import System.Environment (getArgs)
import System.IO

ignore :: Arg Arguments
ignore = flagArg (\_ a -> Right a) ""

e :: String
e = ""

usage = [
    "usage: db migrate [-v|--verbose] [-d|--dry-run]"
  , "       db up [-v|--verbose] [-d|--dry-run]"
  , "       db down [-v|--verbose] [-d|--dry-run]"
  , "       db apply [-v|--verbose] [-d|--dry-run]"
  , "       db -h|--help"
  , "       db -V|--version"
  ]


globalflags :: [Flag Arguments]
globalflags = [
    flagNone [ "h", "help" ]     (\a -> a { adbmode = HelpMode })    e
  , flagNone [ "V", "version" ]  (\a -> a { adbmode = VersionMode }) e
  ]

connectflags :: [Flag Arguments]
connectflags = [
    flagNone [ "v", "verbose" ]    (\a -> a { averbose = True }) e
  , flagNone [ "d", "dry-run" ]    (\a -> a { adry = True }) e
  ]

versionflag = (flagArg (\v a -> Right $ a { aversion = Just v }) "VERSION")

cmdmodes :: String -> Arguments -> Mode Arguments
cmdmodes cmd initial =
  modes cmd initial "" [
      mode "migrate" (initial { adbmode = MigrateMode }) "" ignore connectflags
    , mode "up" (initial { adbmode = UpMode }) "" versionflag connectflags
    , mode "down" (initial { adbmode = DownMode }) "" versionflag connectflags
    , mode "apply" (initial { adbmode = ApplyMode }) "" versionflag connectflags
    , mode "help" (initial { adbmode = HelpMode }) "" ignore []
    , mode "version" (initial { adbmode = VersionMode }) "" ignore []
    ]

data DbMode =
    HelpMode
  | VersionMode
  | MigrateMode
  | UpMode
  | DownMode
  | ApplyMode
  deriving (Eq, Show)

data Arguments = Arguments {
    adbmode :: DbMode
  , adry :: Bool
  , averbose :: Bool
  , ascripts :: String
  , aversion :: Maybe String
  } deriving (Eq, Show)

defaultArguments cwd = Arguments {
    adbmode = HelpMode
  , adry = False
  , averbose = False
  , ascripts = cwd </> "migrations"
  , aversion = Nothing
  }

defaultMain :: Migrations -> MigrateDatabase IO c -> IO c -> IO ()
defaultMain migrationstore db connector = getArgs >>= defaultMain' migrationstore db connector

defaultMain' :: Migrations -> MigrateDatabase IO c -> IO c -> [String] -> IO ()
defaultMain' migrationstore db connector args =
  getCurrentDirectory >>= \cwd ->
    case process ((cmdmodes "migrate" (defaultArguments cwd)) {modeGroupFlags = toGroup $ globalflags} ) args of
      Left x ->  hPutStrLn stderr x >> exitFailure
      Right x -> run migrationstore db connector x

run :: Migrations -> MigrateDatabase IO c -> IO c -> Arguments -> IO ()
run migrationstore db' connector args =
  let db = if adry args then dryrun db' else db'
  in case adbmode args of
    HelpMode -> mapM_ putStrLn usage
    VersionMode -> putStrLn $ "migrate 0.1.1" -- ++ showVersion Program.version
    MigrateMode -> connector  >>= \c -> (executeMigrate migrationstore c $ migrate db) >>= print
    UpMode -> connector >>= \c -> (executeMigrate migrationstore c $ upmode db) >>= print
    DownMode -> connector >>= \c -> (executeMigrate migrationstore c $ downmode db) >>= print
    ApplyMode -> connector >>= \c -> (executeMigrate migrationstore c $ applymode db) >>= print

bomb failwith =
  putStrLn failwith >> exitFailure

upmode :: MigrateDatabase m c -> Migrate c m ()
upmode = undefined

downmode :: MigrateDatabase m c -> Migrate c m ()
downmode = undefined

applymode :: MigrateDatabase m c -> Migrate c m ()
applymode = undefined
