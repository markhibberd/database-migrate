{-#LANGUAGE OverloadedStrings #-}
module Database.Migrate.Demo where

import Control.Monad (forM_)
import Control.Monad.Trans.Either

import Data.Text hiding (find)

import Database.PostgreSQL.Simple
import Database.Migrate

import Debug.Trace

connection :: IO Connection
connection = connect $ defaultConnectInfo { connectUser = "testa", connectPassword = "testa", connectDatabase = "testa" }

runDemoList :: IO ()
runDemoList = do
  c <- connection
  initialize c
  ms <- getMigrations c
  forM_ ms $ \m -> putStrLn $ "migration: " ++ unpack m

runDemoLatest :: IO ()
runDemoLatest = do
  c <- connection
  initialize c
  ems <- runEitherT $ find "/home/mth/work/database-migrate/demo/migrations"
  case ems of
    Left e -> putStrLn e
    Right ms -> putTraceMsg (show (fmap migration ms)) >> runEitherT (latest c ms) >>= \er ->
      case er of
        Left ctx -> putStrLn . show $ ctx
        Right mids -> forM_ mids $ \mid -> putStrLn $ "applied:migration: " ++ unpack mid
