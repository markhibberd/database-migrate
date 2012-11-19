{-#LANGUAGE OverloadedStrings #-}
module Database.Migrate.Demo where

import Control.Monad.Trans.Either

import Data.Text ()

import qualified Database.PostgreSQL.Simple as P
import Database.Migrate

createConnection :: IO P.Connection
createConnection = P.connect $ P.defaultConnectInfo { P.connectUser = "testa", P.connectPassword = "testa", P.connectDatabase = "testa" }

runDemo :: [String] -> IO ()
runDemo args =
  do ems <- runEitherT $ find "migrations"
     case ems of
       Left e -> putStrLn e
       Right ms -> defaultMain' ms psqlMigrateDatabase createConnection args
