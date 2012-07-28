{-#LANGUAGE OverloadedStrings #-}
module Database.Migrate.Demo where

import Control.Monad (forM_)
import Data.Text
import Database.PostgreSQL.Simple
import Database.Migrate
import Data.String (IsString(..))

connection :: IO Connection
connection = connect $ defaultConnectInfo { connectUser = "testa", connectPassword = "testa", connectDatabase = "testa" }

migrations :: [Migration]
migrations = [
    Migration {
      migrationId = MigrationId "1"
    , changes = [
      createTable "test_one" [
        surrogate
      , stdstring "some_col"
      ]
    ]
    }
  ]

runDemoList :: IO ()
runDemoList = do
  c <- connection
  initialize c
  ms <- getMigrations c
  forM_ ms $ \(MigrationId m) ->
    putStrLn $ "migration: " ++ unpack m

runDemoLatest :: IO ()
runDemoLatest = do
  c <- connection
  initialize c
  r <- latest c migrations
  putStrLn $ show r


runDemoScript :: IO ()
runDemoScript = do
  c <- connection
  ddl <- readFile "/home/mth/work/apidocco-server/config/apidocco.ddl"
  x <- execute_ c (fromString ddl)
  putStrLn $ show x
