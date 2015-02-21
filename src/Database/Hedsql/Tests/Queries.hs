module Database.Hedsql.Tests.Queries
    ( tests
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Database.Hedsql.Examples.Select

import Test.Framework                 (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit              hiding (Test)

import qualified Database.Hedsql.SqLite     as S
import qualified Database.Hedsql.PostgreSQL as P

--------------------------------------------------------------------------------
-- PRIVATE
--------------------------------------------------------------------------------

----------------------------------------
-- SELECT
----------------------------------------

testSelectAllSqLite :: Test
testSelectAllSqLite = testCase "Select all" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "Select all query is incorrect"
            "SELECT * FROM \"People\""
            (S.parse selectAll)

testSelectDistinctSqLite :: Test
testSelectDistinctSqLite = testCase "Select distinct" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "Select distinct query is incorrect"
            "SELECT DISTINCT \"firstName\" FROM \"People\""
            (S.parse distinctSelect)

----------------------------------------
-- Functions
----------------------------------------
          
testAdditionSqLite :: Test
testAdditionSqLite = testCase "Addition" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "Addition in query is incorrect"
            "SELECT (\"age\" + 1) FROM \"People\""
            (S.parse addition)
            
testMultiplicationSqLite :: Test
testMultiplicationSqLite = testCase "Multiplication" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "Multiplication in query is incorrect"
            "SELECT (3 * 4)"
            (S.parse multiplication)

testCurrentDateSqLite :: Test
testCurrentDateSqLite = testCase "Current date" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "Current date function in query is incorrect"
            "SELECT Date('now')"
            (S.parse selectCurrentDate)
            
testRandomSqLite :: Test
testRandomSqLite = testCase "Random" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "Random function in query is incorrect"
            "SELECT random()"
            (S.parse selectRandom)

----------------------------------------
-- FROM
----------------------------------------

testCrossJoinSqLite :: Test
testCrossJoinSqLite = testCase "Cross join" assertFrom
    where
        assertFrom :: Assertion
        assertFrom = assertEqual
            "Cross join is incorrect"
            "SELECT * FROM \"People\" CROSS JOIN \"Countries\""
            (S.parse fromCrossJoin)
            
testInnerJoinOnSqLite :: Test
testInnerJoinOnSqLite = testCase "Inner join SqLite" assertFrom
    where
        assertFrom :: Assertion
        assertFrom = assertEqual
            "SqLite inner join is incorrect"
           ("SELECT * "
         ++ "FROM \"People\" "
         ++ "INNER JOIN \"Countries\" "
         ++ "ON \"People\".\"countryId\" = \"Countries\".\"countryId\"")
            (S.parse fromInnerJoinOn)

testInnerJoinUsingSqLite :: Test
testInnerJoinUsingSqLite = testCase "Inner join USING SqLite" assertFrom
    where
        assertFrom :: Assertion
        assertFrom = assertEqual
            "SqLite inner join using is incorrect"
           ("SELECT * "
         ++ "FROM \"People\" INNER JOIN \"Countries\" USING (\"countryId\")")
            (S.parse fromInnerJoinUsing)

testNaturalInnerJoin :: Test
testNaturalInnerJoin = testCase "Natural inner join" assertFrom
    where
        assertFrom :: Assertion
        assertFrom = assertEqual
            "Natural inner join is incorrect"
            "SELECT * FROM \"People\" NATURAL INNER JOIN \"Countries\""
            (S.parse fromNaturalInnerJoin)

testLeftJoinOn :: Test
testLeftJoinOn = testCase "Left join on" assertFrom
    where
        assertFrom :: Assertion
        assertFrom = assertEqual
            "Left join on is incorrect"
            (  "SELECT * FROM \"People\" LEFT JOIN \"Countries\" "
            ++ "ON \"People\".\"countryId\" = \"Countries\".\"countryId\""
            )
            (S.parse fromLeftJoinOn)

testLeftJoinUsing :: Test
testLeftJoinUsing = testCase "Left join using" assertFrom
    where
        assertFrom :: Assertion
        assertFrom = assertEqual
            "Left join using is incorrect"
            (  "SELECT * FROM \"People\" LEFT JOIN \"Countries\" "
            ++ "USING (\"countryId\")"
            )
            (S.parse fromLeftJoinUsing)
 
testRightJoinOn :: Test
testRightJoinOn = testCase "Right join on" assertFrom
    where
        assertFrom :: Assertion
        assertFrom = assertEqual
            "Right join on is incorrect"
            (  "SELECT * FROM \"People\" RIGHT JOIN \"Countries\" "
            ++ "ON \"People\".\"countryId\" = \"Countries\".\"countryId\""
            )
            (S.parse fromRightJoinOn) 

testFullJoinOn :: Test
testFullJoinOn = testCase "Full join on" assertFrom
    where
        assertFrom :: Assertion
        assertFrom = assertEqual
            "Full join on is incorrect"
            (  "SELECT * FROM \"People\" FULL JOIN \"Countries\" "
            ++ "ON \"People\".\"countryId\" = \"Countries\".\"countryId\""
            )
            (S.parse fromFullJoinOn) 

testLeftJoinOnAnd :: Test
testLeftJoinOnAnd = testCase "Left join on and" assertFrom
    where
        assertFrom :: Assertion
        assertFrom = assertEqual
            "Left join on and is incorrect"
            (  "SELECT * FROM \"People\" LEFT JOIN \"Countries\" "
            ++ "ON (\"People\".\"countryId\" = \"Countries\".\"countryId\" "
            ++ "AND \"Countries\".\"name\" = 'Italy')"
            )
            (S.parse fromLeftJoinOnAnd)

testSelfJoin :: Test
testSelfJoin = testCase "Self join" assertFrom
    where
        assertFrom :: Assertion
        assertFrom = assertEqual
            "Self join is incorrect"
            (  "SELECT * FROM \"People\" AS \"Father\" "
            ++ "INNER JOIN \"People\" AS \"Child\" "
            ++ "ON \"Father\".\"personId\" = \"Child\".\"father\""
            )
            (S.parse selfJoin)

testCrossJoinAlias :: Test
testCrossJoinAlias = testCase "Cross join with aliases" assertFrom
    where
        assertFrom :: Assertion
        assertFrom = assertEqual
            "Cross join with aliases is incorrect"
            (  "SELECT * FROM \"People\" AS \"P\" "
            ++ "CROSS JOIN \"Countries\" AS \"C\""
            )
            (S.parse crossJoinAlias)

testCrossRefAlias :: Test
testCrossRefAlias = testCase "Cross join alias reference" assertFrom
    where
        assertFrom :: Assertion
        assertFrom = assertEqual
            "Cross join alias reference is incorrect"
            (  "SELECT * FROM (\"People\" AS \"P\" "
            ++ "CROSS JOIN \"Countries\") AS \"PC\""
            )
            (S.parse crossRefAlias)

testSubQuery :: Test
testSubQuery = testCase "Sub-query in FROM clause" assertFrom
    where
        assertFrom :: Assertion
        assertFrom = assertEqual
            "Sub-query in FROM clause is incorrect"
            "SELECT * FROM (SELECT * FROM \"People\") AS \"P\""
            (S.parse selectSubQuery)

----------------------------------------
-- WHERE
----------------------------------------

testWhereAlias :: Test
testWhereAlias = testCase "WHERE clause with aliases" assertFrom
    where
        assertFrom :: Assertion
        assertFrom = assertEqual
            "WHERE clause with aliases is incorrect"
            "SELECT * FROM \"People\" AS \"P\" WHERE \"P\".\"age\" > 5"
            (S.parse whereAlias)

testLeftJoinWhere :: Test
testLeftJoinWhere = testCase "Left join with WHERE clause" assertFrom
    where
        assertFrom :: Assertion
        assertFrom = assertEqual
            "Left join with WHERE clause is incorrect"
            (  "SELECT * FROM \"People\" LEFT JOIN \"Countries\" "
            ++ "ON \"People\".\"countryId\" = \"Countries\".\"countryId\" "
            ++ "WHERE \"Countries\".\"name\" = 'Italy'"
            )
            (S.parse leftJoinWhere)

testWhereAnd :: Test
testWhereAnd = testCase "WHERE clause with AND" assertFrom
    where
        assertFrom :: Assertion
        assertFrom = assertEqual
            "WHERE clause with AND is incorrect"
            (  "SELECT * FROM \"People\", \"Countries\" "
            ++ "WHERE \"People\".\"countryId\" = \"Countries\".\"countryId\" "
            ++ "AND \"People\".\"age\" > 18"
            )
            (S.parse whereAnd)

testWhereInValues :: Test
testWhereInValues = testCase "WHERE clause with IN values" assertFrom
    where
        assertFrom :: Assertion
        assertFrom = assertEqual
            "WHERE clause with IN values is incorrect"
            (  "SELECT * FROM \"Countries\" "
            ++ "WHERE \"name\" IN ('Italy', 'Switzerland')"
            )
            (S.parse whereInValues)

testWhereInSelect :: Test
testWhereInSelect = testCase "WHERE clause with IN sub-query" assertFrom
    where
        assertFrom :: Assertion
        assertFrom = assertEqual
            "WHERE clause with IN sub-query is incorrect"
            (  "SELECT * FROM \"People\" "
            ++ "WHERE \"countryId\" IN (SELECT \"countryId\" "
            ++ "FROM \"Countries\" WHERE \"inhabitants\" >= (\"size\" * 100))"
            )
            (S.parse whereInSelect)

testWhereBetween :: Test
testWhereBetween = testCase "WHERE clause with BETWEEN clause" assertFrom
    where
        assertFrom :: Assertion
        assertFrom = assertEqual
            "WHERE clause with BETWEEN clause is incorrect"
            (  "SELECT * FROM \"Countries\" "
            ++ "WHERE \"inhabitants\" BETWEEN (SELECT \"age\" "
            ++ "FROM \"People\" WHERE \"firstName\" LIKE '*e*') AND 1000000"
            )
            (S.parse whereBetween)
 
testWhereExists :: Test
testWhereExists = testCase "WHERE clause with EXISTS sub-query" assertFrom
    where
        assertFrom :: Assertion
        assertFrom = assertEqual
            "WHERE clause with EXISTS sub-query is incorrect"
            (  "SELECT * FROM \"People\" "
            ++ "WHERE EXISTS (SELECT * "
            ++ "FROM \"Countries\" "
            ++ "WHERE \"People\".\"countryId\" = \"Countries\".\"countryId\")"
            )
            (S.parse whereExists)

----------------------------------------
-- GROUP By
----------------------------------------

testGroupBy :: Test
testGroupBy = testCase "GROUP BY clause" assertGroupBy
    where
        assertGroupBy :: Assertion
        assertGroupBy = assertEqual
            "GROUP BY clause is incorrect"
            "SELECT \"age\" FROM \"People\" GROUP BY \"age\""
            (S.parse selectGroupBy)

testGroupBySum :: Test
testGroupBySum = testCase "GROUP BY with SUM" assertGroupBy
    where
        assertGroupBy :: Assertion
        assertGroupBy = assertEqual
            "GROUP BY with SUM is incorrect"
            (  "SELECT \"lastName\", SUM(\"age\") FROM \"People\" "
            ++ "GROUP BY \"lastName\""
            )
            (S.parse groupBySum)

testGroupByAlias :: Test
testGroupByAlias = testCase "GROUP BY with an alias" assertGroupBy
    where
        assertGroupBy :: Assertion
        assertGroupBy = assertEqual
            "GROUP BY with an alias is incorrect"
            (  "SELECT \"lastName\" AS \"name\" FROM \"People\" "
            ++ "GROUP BY \"name\""
            )
            (S.parse groupByAlias)

testGroupByComplex :: Test
testGroupByComplex = testCase "Complex GROUP BY" assertGroupBy
    where
        assertGroupBy :: Assertion
        assertGroupBy = assertEqual
            "Complex GROUP BY is invalid"
            (  "SELECT \"personId\", \"P\".\"lastName\" AS \"name\", "
            ++ "(SUM(\"C\".\"size\") * \"P\".\"age\") AS \"weirdFigure\" "
            ++ "FROM \"People\" AS \"P\" LEFT JOIN \"Countries\" AS \"C\" "
            ++ "USING (\"personId\") GROUP BY \"personId\", \"P\".\"name\""
            )
            (S.parse groupByComplex)

testGroupBySumHaving :: Test
testGroupBySumHaving = testCase "GROUP BY with SUM and HAVING" assertGroupBy
    where
        assertGroupBy :: Assertion
        assertGroupBy = assertEqual
            "GROUP BY with SUM and HAVING is incorrect"
            (  "SELECT \"lastName\", SUM(\"age\") "
            ++ "FROM \"People\" GROUP BY \"lastName\" HAVING SUM(\"age\") > 18"
            )
            (S.parse groupBySumHaving)

testHavingComplex :: Test
testHavingComplex = testCase "Complex HAVING" assertGroupBy
    where
        assertGroupBy :: Assertion
        assertGroupBy = assertEqual
            "Complex HAVING is invalid"
            (  "SELECT \"personId\", \"P\".\"name\", "
            ++ "SUM(\"C\".\"size\" * (\"P\".\"age\" - 2)) AS \"weird\" "
            ++ "FROM \"People\" AS \"P\" LEFT JOIN \"Countries\" AS \"C\" "
            ++ "USING (\"personId\") WHERE \"personId\" > 2 "
            ++ "GROUP BY \"personId\", \"P\".\"name\", \"P\".\"age\" "
            ++ "HAVING SUM(\"P\".\"age\" * \"C\".\"size\") > 5000000"
            )
            (S.parse havingComplex)

----------------------------------------
-- Combined queries
----------------------------------------

testUnion :: Test
testUnion = testCase "SELECT UNION" assertUnion
    where
        assertUnion :: Assertion
        assertUnion = assertEqual
            "SELECT UNION is incorrect"
            (  "(SELECT * FROM \"People\" WHERE \"personId\" = 1 "
            ++ "UNION SELECT * FROM \"People\" WHERE \"personId\" = 2)"
            )
            (S.parse unionQuery)

testUnionCombined :: Test
testUnionCombined = testCase "Combined SELECT UNIONs" assertUnion
    where
        assertUnion :: Assertion
        assertUnion = assertEqual
            "Combined SELECT UNIONs are incorrect"
            (  "((SELECT * FROM \"People\" WHERE \"personId\" = 1 "
            ++ "UNION SELECT * FROM \"People\" WHERE \"personId\" = 2) "
            ++ "INTERSECT SELECT * FROM \"People\" WHERE \"personId\" = 1)"
            )
            (S.parse unionCombined)
            
----------------------------------------
-- PostgreSQL
----------------------------------------

testSelectDistinctOnPostgreSQL :: Test
testSelectDistinctOnPostgreSQL = testCase "Select distinct on" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "Select distinct on query is incorrect"
           ("SELECT DISTINCT ON (\"firstName\") * "
         ++ "FROM \"People\" ORDER BY \"age\"")
            (P.parse distinctOnSelect)

testFromLateralPostgreSQL :: Test
testFromLateralPostgreSQL = testCase "Lateral join" assertSelect
    where
        assertSelect :: Assertion
        assertSelect = assertEqual
            "Lateral join is incorrect"
            (  "SELECT * "
            ++ "FROM \"Countries\", LATERAL ("
            ++ "SELECT * FROM \"People\" "
            ++ "WHERE \"People\".\"countryId\" = \"Countries\".\"countryId\") "
            ++ "AS \"C\""
            )
            (P.parse fromLateral)

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

-- | Gather all tests.
tests :: Test
tests = testGroup "Select"
    [ testGroup "All vendors"
        [ testSelectAllSqLite
        , testSelectDistinctSqLite
        , testAdditionSqLite
        , testCurrentDateSqLite
        , testMultiplicationSqLite
        , testCrossJoinSqLite
        , testInnerJoinOnSqLite
        , testInnerJoinUsingSqLite
        , testNaturalInnerJoin
        , testLeftJoinOn
        , testLeftJoinUsing
        , testRightJoinOn
        , testFullJoinOn
        , testLeftJoinOnAnd
        , testSelfJoin
        , testCrossJoinAlias
        , testCrossRefAlias
        , testSubQuery
        , testWhereAlias
        , testLeftJoinWhere
        , testWhereAnd
        , testWhereInValues
        , testWhereInSelect
        , testWhereBetween
        , testWhereExists
        , testGroupBy
        , testGroupBySum
        , testGroupByAlias
        , testGroupByComplex
        , testGroupBySumHaving
        , testHavingComplex
        , testUnion
        , testUnionCombined
        , testRandomSqLite
        ]
    , testGroup "PostgreSQL"
        [ testSelectDistinctOnPostgreSQL
        , testFromLateralPostgreSQL
        ]
    ]