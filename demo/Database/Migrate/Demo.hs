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
     c <- createConnection
     case ems of
       Left e -> putStrLn e
       Right ms -> defaultMain' ms psqlMigrateDatabase (return c) args >> P.close c
