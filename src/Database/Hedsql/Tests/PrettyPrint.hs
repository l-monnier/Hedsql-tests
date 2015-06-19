module Database.Hedsql.Tests.PrettyPrint
    ( tests
    ) where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Database.Hedsql.Examples.Select

import Test.Framework                 (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit              hiding (Test)

import qualified Database.Hedsql.SqLite     as S

--------------------------------------------------------------------------------
-- PRIVATE
--------------------------------------------------------------------------------

testSelectStruct :: Test
testSelectStruct = testCase "Basic SELECT structure" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "SELECT structure is incorrect."
            "SELECT \"firstName\" FROM \"People\" WHERE \"age\" > 18"
            (S.parse selectGen)

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

-- | Gather all tests.
tests :: Test
tests = testGroup "Pretty Print"
    [ testSelectStruct
    ]