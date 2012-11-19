module Main where

import System.Environment
import Database.Migrate.Demo


main ::
  IO ()
main =
  runDemoDryRun
