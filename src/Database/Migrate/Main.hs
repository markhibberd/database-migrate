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
    flagNone [ "h", "help" ]     (\a -> a { adbmode = HelpMode })    e
  , flagNone [ "V", "version" ]  (\a -> a { adbmode = VersionMode }) e
  ]

connectflags :: [Flag Arguments]
connectflags = [
    flagReq  [ "H", "hostname" ]   (\v a -> Right $ a { ainfo = (ainfo a) { hostname = Just v } }) e e
  , flagReq  [ "P", "password" ]   (\v a -> Right $ a { ainfo = (ainfo a) { password = Just v } }) e e
  , flagReq  [ "p", "port" ]       (\v a ->
                                     case reads v of
                                       [(i, "")] -> Right $ a { ainfo = (ainfo a) { port = Just i } }
                                       _ -> Left "invalid port number"
                                   ) e e

  , flagReq  [ "u", "user" ]       (\v a -> Right $ a { ainfo = (ainfo a) { user = Just v } }) e e
  , flagReq  [ "D", "db" ]         (\v a -> Right $ a { ainfo = (ainfo a) { dbname = Just v } }) e e
  , flagNone [ "g", "postgres" ]   (\a -> a { ainfo = (ainfo a) { conntype = PostgresConnType  } }) e
  , flagNone [ "m", "mysql" ]      (\a -> a { ainfo = (ainfo a) { conntype = MysqlConnType } }) e
  , flagNone [ "v", "verbose" ]    (\a -> a { averbose = True }) e
  , flagNone [ "d", "dry-run" ]    (\a -> a { adry = True }) e
  , flagReq  [ "s", "scripts" ]    (\v a -> Right $ a { ascripts = v }) e e
  ]

versionflag = (flagArg (\v a -> Right $ a { aversion = Just v }) "VERSION")

cmdmodes :: String -> Arguments -> Mode Arguments
cmdmodes cmd initial =
  modes cmd initial "" [
      mode "migrate" (initial { adbmode = MigrateMode }) "" ignore connectflags
    , mode "up" (initial { adbmode = UpMode }) "" versionflag connectflags
    , mode "down" (initial { adbmode = DownMode }) "" versionflag connectflags
    , mode "apply" (initial { adbmode = ApplyMode }) "" versionflag connectflags
    , mode "test" (initial { adbmode = TestMode }) "" versionflag connectflags
    , mode "info" (initial { adbmode = InfoMode }) "" ignore connectflags
    , mode "help" (initial { adbmode = HelpMode }) "" ignore []
    , mode "version" (initial { adbmode = VersionMode }) "" ignore []
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
    adbmode :: DbMode
  , adry :: Bool
  , averbose :: Bool
  , ascripts :: String
  , ainfo :: MigrateConnectInfo
  , aversion :: Maybe String
  } deriving (Eq, Show)

defaultArguments cwd = Arguments {
    adbmode = HelpMode
  , adry = False
  , averbose = False
  , ascripts = cwd </> "migrations"
  , ainfo = MigrateConnectInfo Nothing Nothing Nothing Nothing Nothing PostgresConnType
  , aversion = Nothing
  }

defaultMain cmd =
  getCurrentDirectory >>= \cwd -> processArgs ((cmdmodes cmd (defaultArguments cwd)) {modeGroupFlags = toGroup $ globalflags} )>>= run putStrLn

run logger args =
  case adbmode args of
    HelpMode -> mapM_ putStrLn usage
    VersionMode -> putStrLn $ "migrate " ++ showVersion Program.version
    MigrateMode -> buildconnnection (ainfo args)  >>= migratemode logger (ascripts args)
    UpMode -> buildconnnection (ainfo args) >>= upmode
    DownMode -> buildconnnection (ainfo args) >>= downmode
    ApplyMode -> buildconnnection (ainfo args) >>= applymode
    TestMode -> buildconnnection (ainfo args) >>= testmode
    InfoMode -> buildconnnection (ainfo args) >>= infomode

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



migratemode :: MigrateDatabase IO a => l -> s -> a -> IO ()
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

