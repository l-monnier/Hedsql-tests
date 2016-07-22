{-# LANGUAGE OverloadedStrings #-}

module Database.Hedsql.Tests.Delete
    ( tests
    ) where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Data.Monoid
import Database.Hedsql.Examples.Delete

import Test.Framework                 (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit              hiding (Test)

import qualified Database.Hedsql.SqLite     as S
import qualified Database.Hedsql.MariaDB    as M
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
            <> "WHERE \"personId\" IN "
            <> "(SELECT \"personId\" "
            <> "FROM \"Countries\" "
            <> "WHERE \"name\" = 'Switzerland')"
            )
            (S.parse deleteSubQuery)

----------------------------------------
-- PostgreSQL
----------------------------------------

testReturningPostgreSQL :: Test
testReturningPostgreSQL =
    testCase "Delete with RETURNING clause for PostgreSQL" assertDelete
    where
        assertDelete :: Assertion
        assertDelete = assertEqual
            "Delete with RETURNING for PostgreSQL is incorrect"
            (  "DELETE FROM \"People\" "
            <> "WHERE \"age\" = 20 "
            <> "RETURNING \"personId\""
            )
            (P.parse deleteReturningClause)

----------------------------------------
-- MariaDB
----------------------------------------

testReturningMariaDB :: Test
testReturningMariaDB =
    testCase "Delete with RETURNING clause for MariaDB" assertDelete
    where
        assertDelete :: Assertion
        assertDelete = assertEqual
            "Delete with RETURNING for MariaDB is incorrect"
            (  "DELETE FROM \"People\" "
            <> "WHERE \"age\" = 20 "
            <> "RETURNING \"personId\""
            )
            (M.parse deleteReturningClauseMariaDB)

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

-- | Gather all tests.
tests :: Test
tests = testGroup "Delete"
    [ testGroup "AllVendors"
        [ testNotEqualTo
        , testSubQuery
        ]
    , testGroup "PostgreSQL"
        [ testReturningPostgreSQL
        ]
    , testGroup "MariaDB"
        [ testReturningMariaDB
        ]
    ]
