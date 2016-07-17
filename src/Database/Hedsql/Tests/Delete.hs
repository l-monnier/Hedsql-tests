module Database.Hedsql.Tests.Delete
    ( tests
    ) where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Database.Hedsql.Examples.Delete

import Test.Framework                 (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit              hiding (Test)

import qualified Database.Hedsql.SqLite     as S
import qualified Database.Hedsql.PostgreSQL as P

--------------------------------------------------------------------------------
-- PRIVATE
--------------------------------------------------------------------------------

testNotEqualTo :: Test
testNotEqualTo = testCase "Delete with not-equal to" assertDelete
    where
        assertDelete :: Assertion
        assertDelete = assertEqual
            "Delete with not-equal to is incorrect"
            "DELETE FROM \"People\" WHERE \"age\" <> 20"
            (S.parse deleteNotEqualTo)

testSubQuery :: Test
testSubQuery = testCase "Delete with sub-query" assertDelete
    where
        assertDelete :: Assertion
        assertDelete = assertEqual
            "Delete with a sub-query is incorrect"
            (  "DELETE FROM \"People\" "
            ++ "WHERE \"personId\" IN "
            ++ "(SELECT \"personId\" "
            ++ "FROM \"Countries\" "
            ++ "WHERE \"name\" = 'Switzerland')"
            )
            (S.parse deleteSubQuery)

testReturning :: Test
testReturning = testCase "Delete with RETURNING clause" assertDelete
    where
        assertDelete :: Assertion
        assertDelete = assertEqual
            "Delete with RETURNING is incorrect"
            (  "DELETE FROM \"People\" "
            ++ "WHERE \"age\" = 20 "
            ++ "RETURNING \"personId\""
            )
            (P.parse deleteReturningClause)

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

-- | Gather all tests.
tests :: Test
tests = testGroup "Delete"
    [ testNotEqualTo
    , testSubQuery
    , testReturning
    ]
