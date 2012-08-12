import qualified Paths_database_migrate as Program (version)

import Data.Version (showVersion)
import System.Console.CmdArgs.Explicit
import System.Exit

usage :: [String]
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
  } deriving (Eq, Show)

defaultArguments :: Arguments
defaultArguments = Arguments {
    printhelp = False
  , printversion = False
  , dry = False
  , test = False
  , auto = False
  , postgres = Nothing
  , mysql = Nothing
  , version = Nothing
  }

migratemode :: Mode Arguments
migratemode =
  mode "migrate" defaultArguments "" (flagArg (\v a -> Right $ a { version = Just v }) "VERSION") [
      flagNone [ "h", "help" ]     (\a -> a { printhelp = True })    "print help and exit"
    , flagNone [ "V", "version" ]  (\a -> a { printversion = True }) "print version and exit"
    , flagNone [ "d", "dry-run" ]  (\a -> a { dry = True })          "do not perform any updates"
    ]

migrate :: IO ()
migrate = undefined

run :: Arguments -> IO ()
run args
  | printhelp args      = mapM_ putStrLn usage
  | printversion args   = putStrLn $ "migrate " ++ showVersion Program.version
  | otherwise           = migrate

main :: IO ()
main = processArgs migratemode >>= run
