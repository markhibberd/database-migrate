module Main where

import qualified Database.Migrate.Tests
import Test.Framework

main ::
  IO ()
main = 
  defaultMain tests 

tests ::
  [Test]
tests =
  [
    testGroup "Tests"
      [
        Database.Migrate.Tests.test
      ]
  ]

