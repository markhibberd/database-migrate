import qualified Paths_database_migrate as Program (version)

import Control.Monad (mapM_)
import Data.Version (showVersion)
import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Text
import System.Exit

usage :: [String]
usage = [
    "usage: migrate ..."
  , "       migrate -h|--help"
  , "       migrate -v|--version"
  ]

data Arguments = Arguments {
    help :: Bool
  , version :: Bool
  } deriving (Eq, Show)

defaultArguments :: Arguments
defaultArguments = Arguments False False

ignore :: Arg Arguments
ignore = flagArg (\_ a -> Right a) ""

migratemode :: Mode Arguments
migratemode =
  mode "migrate" defaultArguments "" ignore [
      flagNone [ "h", "help" ]     (\a -> a { help = True })    "print help and exit"
    , flagNone [ "V", "version" ]  (\a -> a { version = True }) "print version and exit"
    ]

migrate :: IO ()
migrate = undefined

run :: Arguments -> IO ()
run args
  | help args      = mapM_ putStrLn usage
  | version args   = putStrLn $ "migrate " ++ showVersion Program.version
  | otherwise      = migrate

main :: IO ()
main = processArgs migratemode >>= run
