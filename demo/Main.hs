module Main where

import System.Environment
import Database.Migrate.Demo


main :: IO ()
main = getArgs >>= main'

main' :: [String] -> IO ()
main' = runDemo


