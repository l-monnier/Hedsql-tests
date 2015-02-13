module Database.Hedsql.Tests.Update
    ( tests
    ) where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Database.Hedsql.Examples.Update

import Test.Framework                 (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit              hiding (Test)

import qualified Database.Hedsql.PostgreSQL as P
import qualified Database.Hedsql.SqLite     as S

--------------------------------------------------------------------------------
-- PRIVATE
--------------------------------------------------------------------------------

----------------------------------------
-- All vendors
----------------------------------------

testEqualTo :: Test
testEqualTo = testCase "Update with equal-to" assertUpdate
    where
        assertUpdate :: Assertion
        assertUpdate = assertEqual
            "Update with equal-to is incorrect"
            "UPDATE \"People\" SET \"age\" = 2050 WHERE \"lastName\" = 'Ceasar'"
            (S.parse equalTo)

testUpdateSelect :: Test
testUpdateSelect = testCase "Update with a SELECT" assertUpdate
    where
        assertUpdate :: Assertion
        assertUpdate = assertEqual
            "Update with a SELECT is incorrect"
            (  "UPDATE \"People\" SET \"age\" = (\"age\" + 1) "
            ++ "WHERE \"countryId\" IN "
            ++ "(SELECT \"countryId\" FROM \"countries\" "
            ++ "WHERE \"name\" = 'Italy')"
            )
            (S.parse updateSelect)

----------------------------------------
-- PostgreSQL
----------------------------------------

testDefaultVal :: Test
testDefaultVal = testCase "Update with defaultValue" assertUpdate
    where
        assertUpdate :: Assertion
        assertUpdate = assertEqual
            "Update with a default value is incorrect"
            "UPDATE \"People\" SET \"title\" = DEFAULT WHERE \"personId\" = 1"
            (P.parse defaultVal)

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

-- | Gather all tests.
tests :: Test
tests = testGroup "Update"
    [ testGroup "All vendors"
        [ testEqualTo
        , testUpdateSelect
        ]
    , testGroup "PostgreSQL"
        [ testDefaultVal
        ]
    ]