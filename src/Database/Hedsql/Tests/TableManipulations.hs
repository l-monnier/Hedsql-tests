module Database.Hedsql.Tests.TableManipulations
    ( tests
    ) where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Database.Hedsql.Examples.Create
import Database.Hedsql.Examples.Drop

import qualified Database.Hedsql.SqLite     as S
import qualified Database.Hedsql.PostgreSQL as P

import Test.Framework                 (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit              hiding (Test)

--------------------------------------------------------------------------------
-- PRIVATE
--------------------------------------------------------------------------------

----------------------------------------
-- PostgreSQL
----------------------------------------

--------------------
-- Full examples
--------------------

testCountriesPostgreSQL :: Test
testCountriesPostgreSQL =
    testCase "Create table \"Countries\" for PostgreSQL" assertCreate
    where
        assertCreate :: Assertion
        assertCreate = assertEqual
            "Create table \"Countries\" is incorrect for PostgreSQL"
            (  "CREATE TABLE \"Countries\" ("
            ++ "\"countryId\" serial PRIMARY KEY, "
            ++ "\"name\" varchar(256) NOT NULL, UNIQUE, "
            ++ "\"size\" integer, "
            ++ "\"inhabitants\" integer)"
            )
            (P.parse countries)

testPeoplePostgreSQL :: Test
testPeoplePostgreSQL =
    testCase "Create table \"People\" for PostgreSQL" assertCreate
    where
        assertCreate :: Assertion
        assertCreate = assertEqual
            "Create table \"People\" is incorrect for PostgreSQL"
            (  "CREATE TABLE \"People\" ("
            ++ "\"personId\" serial PRIMARY KEY, "
            ++ "\"title\" char(2) DEFAULT('Ms'), "
            ++ "\"firstName\" varchar(256) NOT NULL, "
            ++ "\"lastName\" varchar(256) NOT NULL, "
            ++ "\"age\" integer CHECK (\"age\" > -1), "
            ++ "\"passportNo\" varchar(256) UNIQUE, "
            ++ "\"father\" integer REFERENCES \"People\"(\"personId\"), "
            ++ "\"countryId\" integer REFERENCES \"Countries\"(\"countryId\"))"
            )
            (P.parse people)

--------------------
-- Primary key
--------------------

testPrimaryKeyPostgreSQL :: Test
testPrimaryKeyPostgreSQL = testCase "Create table with primary key" assertCreate
    where
        assertCreate :: Assertion
        assertCreate = assertEqual
            "Create table with a primary key is incorrect for PostgreSQL"
            "CREATE TABLE \"People\" (\"personId\" integer PRIMARY KEY)"
            (P.parse primaryKeyCol)

----------------------------------------
-- SQLite
----------------------------------------

--------------------
-- Full examples
--------------------

testCountriesSqLite :: Test
testCountriesSqLite =
    testCase "Create table \"Countries\" for SqLite" assertCreate
    where
        assertCreate :: Assertion
        assertCreate = assertEqual
            "Create table \"Countries\" is incorrect for SqLite"
            (  "CREATE TABLE \"Countries\" ("
            ++ "\"countryId\" INTEGER PRIMARY KEY AUTOINCREMENT, "
            ++ "\"name\" VARCHAR(256) NOT NULL, UNIQUE, "
            ++ "\"size\" INTEGER, "
            ++ "\"inhabitants\" INTEGER)"
            )
            (S.parse countries)

testPeopleSqLite :: Test
testPeopleSqLite = testCase "Create table \"People\" for SqLite" assertCreate
    where
        assertCreate :: Assertion
        assertCreate = assertEqual
            "Create table \"People\" is incorrect for SqLite"
            (  "CREATE TABLE \"People\" ("
            ++ "\"personId\" INTEGER PRIMARY KEY AUTOINCREMENT, "
            ++ "\"title\" CHARACTER(2) DEFAULT('Ms'), "
            ++ "\"firstName\" VARCHAR(256) NOT NULL, "
            ++ "\"lastName\" VARCHAR(256) NOT NULL, "
            ++ "\"age\" INTEGER CHECK (\"age\" > -1), "
            ++ "\"passportNo\" VARCHAR(256) UNIQUE, "
            ++ "\"father\" INTEGER REFERENCES \"People\"(\"personId\"), "
            ++ "\"countryId\" INTEGER REFERENCES \"Countries\"(\"countryId\"))"
            )
            (S.parse people)

--------------------
-- Primary key
--------------------

testPrimaryKeySqLite :: Test
testPrimaryKeySqLite =
    testCase "Create table with primary key" assertCreate
    where
        assertCreate :: Assertion
        assertCreate = assertEqual
            "Create table with a primary key is incorrect for SqLite"
            "CREATE TABLE \"People\" (\"personId\" INTEGER PRIMARY KEY)"
            (S.parse primaryKeyCol)
           
testPrimaryKeyAutoSqLite :: Test
testPrimaryKeyAutoSqLite =
    testCase "Create table with primary key and auto increment" assertCreate
    where
        assertCreate :: Assertion
        assertCreate = assertEqual
            (  "Create table with a primary key with an auto increment"
            ++ "is incorrect for SqLite"
            )
            (  "CREATE TABLE \"People\" (\"personId\" INTEGER PRIMARY KEY "
            ++ "AUTOINCREMENT)"
            )
            (S.parse primaryKeyColAuto)
            
testPrimaryKeyTableSqLite :: Test
testPrimaryKeyTableSqLite = testCase "Create table with primary key" assertCreate
    where
        assertCreate :: Assertion
        assertCreate = assertEqual
            "Create table with a primary key is incorrect for SqLite"
           ("CREATE TABLE \"People\" ("
         ++ "\"firstName\" VARCHAR(256), "
         ++ "\"lastName\" VARCHAR(256), "
         ++ "CONSTRAINT \"pk\" PRIMARY KEY (\"firstName\", \"lastName\"))")
            (S.parse primaryKeyTable)

testPrimaryKeyAutoPostgreSQL :: Test
testPrimaryKeyAutoPostgreSQL =
    testCase "Create table with primary key with auto increment" assertCreate
    where
        assertCreate :: Assertion
        assertCreate = assertEqual
            ("Create table with a primary key with auto increment"
          ++ "is incorrect for PostgreSQL")
            "CREATE TABLE \"People\" (\"personId\" serial PRIMARY KEY)"
            (P.parse primaryKeyColAuto)

testDefaultValSqLite :: Test
testDefaultValSqLite = testCase "Create table with a default value" assertCreate
    where
        assertCreate :: Assertion
        assertCreate = assertEqual
            "Create table with a default value"
            "CREATE TABLE \"People\" (\"country\" INTEGER DEFAULT(1))"
            (S.parse defaultVal)
            
testNoNullsSqLite :: Test
testNoNullsSqLite =
    testCase "Create table with not null constraints" assertCreate
    where
        assertCreate :: Assertion
        assertCreate = assertEqual
            "Create table with not null constraints"
            ("CREATE TABLE \"People\" ("
          ++ "\"firstName\" VARCHAR(256) CONSTRAINT \"no_null\" NOT NULL, "
          ++ "\"lastName\" VARCHAR(256) NOT NULL)")
            (S.parse noNulls)

testCreateCheckSqLite :: Test
testCreateCheckSqLite = testCase "Create table with check" assertCreate
    where
        assertCreate :: Assertion
        assertCreate = assertEqual
            "Check constraint in table statement is incorrect"
            "CREATE TABLE \"People\" (\"age\" INTEGER CHECK (\"age\" > -1))"
            (S.parse createCheck)

testCreateChecksSqLite :: Test
testCreateChecksSqLite =
    testCase "Create table with many checks" assertCreate
    where
        assertCreate :: Assertion
        assertCreate = assertEqual
            "Check constraints in table statement are incorrect"
           ("CREATE TABLE \"People\" ("
         ++ "\"lastName\" VARCHAR(256), \"age\" INTEGER, "
         ++ "CONSTRAINT \"checks\" CHECK (\"age\" > -1 AND \"lastName\" <> '')"
         ++ ")")
           (S.parse createChecks)

testCreateFKSqLite :: Test
testCreateFKSqLite =
    testCase "Create table with a foreign key" assertCreate
    where
        assertCreate :: Assertion
        assertCreate = assertEqual
            "Foreign key in table statement is incorrect"
           ("CREATE TABLE \"People\" "
         ++ "(\"countryId\" INTEGER REFERENCES \"Countries\"(\"countryId\"))")
           (S.parse createFK)

testCreateTableSqLite :: Test
testCreateTableSqLite = testCase "Create table" assertCreate
    where
        assertCreate :: Assertion
        assertCreate = assertEqual
            "Create table statement is incorrect"
            "CREATE TABLE \"People\" (\"firstName\" VARCHAR(256))"
            (S.parse simpleTable)

testCreateUniqueSqLite :: Test
testCreateUniqueSqLite =
    testCase "Create table with unique constraint" assertCreate
    where
        assertCreate :: Assertion
        assertCreate = assertEqual
            "Create table with unique constraint is incorrect"
            "CREATE TABLE \"People\" (\"passportNo\" VARCHAR(256) UNIQUE)"
            (S.parse createUnique)
            
testCreateUniqueTSqLite :: Test
testCreateUniqueTSqLite =
    testCase "Create table with unique constraint on two columns" assertCreate
    where
        assertCreate :: Assertion
        assertCreate = assertEqual
            "Create table with unique constraint on two columns is incorrect"
           ("CREATE TABLE \"People\" (\"firstName\" VARCHAR(256), "
         ++ "\"lastName\" VARCHAR(256), UNIQUE (\"firstName\", \"lastName\"))")
            (S.parse createUniqueT)
            
--------------------
-- DROP statements
--------------------

testDropTable :: Test
testDropTable = testCase "Drop a table" assertDrop
    where
        assertDrop :: Assertion
        assertDrop = assertEqual
            "Drop table is incorrect for SqLite"
            "DROP TABLE \"People\""
            (S.parse dropTableStmt)
            
testDropTableIfExists :: Test
testDropTableIfExists = testCase "Drop a table if it exists" assertDrop
    where
        assertDrop :: Assertion
        assertDrop = assertEqual
            "Drop table if table exists is incorrect for SqLite"
            "DROP TABLE IF EXISTS \"People\""
            (S.parse dropTableIfExistsStmt)
            
--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------
            
-- | Gather all tests.
tests :: Test
tests = testGroup "Table manipulations"
    [ testGroup "PostgreSQL"
        [ testGroup "Full examples"
            [ testPeoplePostgreSQL
            , testCountriesPostgreSQL
            ]
        , testGroup "Create tables"
            [ testPrimaryKeyAutoPostgreSQL
            , testPrimaryKeyPostgreSQL
            ]
        ]
    , testGroup "All vendors"
        [ testGroup "Full examples"
            [ testCountriesSqLite
            , testPeopleSqLite
            ]
        , testGroup "Create tables"
            [ testCreateCheckSqLite
            , testCreateChecksSqLite
            , testCreateFKSqLite
            , testCreateTableSqLite
            , testCreateUniqueSqLite
            , testCreateUniqueTSqLite
            , testDefaultValSqLite
            , testNoNullsSqLite
            , testPrimaryKeySqLite
            , testPrimaryKeyAutoSqLite
            , testPrimaryKeyTableSqLite
            ]
        , testGroup "Drop statements"
            [ testDropTable
            , testDropTableIfExists
            ]
        ]
    ]