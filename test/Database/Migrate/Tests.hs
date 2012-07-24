module Database.Migrate.Tests
  (
    main
  , test
  ) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Database.Migrate

main ::
  IO ()
main =
  defaultMain [test]

test ::
  Test
test =
    testGroup "Migrate"
      [
        testProperty "Right Identity" prop_right_identity
      ]

prop_right_identity ::
  Int
  -> Bool
prop_right_identity n =
  n `add` 0 == n

