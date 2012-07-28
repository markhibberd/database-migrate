module Main where

import System.Environment
import Database.Migrate.Demo


main ::
  IO ()
main =
  getArgs >>= \args ->
    if args == ["update"]
      then runDemoLatest
      else runDemoList


