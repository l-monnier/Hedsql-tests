{-|
Module      : Hedsql/Common/Parser/Queries/Select.hs
Description : Collection of SELECT queries.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

A collection of SELECT queries to be used in tests or as examples.
-}
module Database.Hedsql.Examples.Select
    ( 
      -- * All DB vendors
      
      -- ** SELECT
      selectAll
    , distinctSelect
    
      -- ** FROM
      
      -- *** Joins
    , fromCrossJoin
    , fromInnerJoinOn
    , fromInnerJoinUsing
    , fromNaturalInnerJoin
    , fromLeftJoinOn
    , fromLeftJoinUsing
    , fromRightJoinOn
    , fromFullJoinOn
    , fromLeftJoinOnAnd
    , selfJoin
    , crossJoinAlias
    , crossRefAlias
    
      -- *** Sub-queries
    , selectSubQuery
    
      -- ** WHERE
    , whereAlias
    , leftJoinWhere
    , whereAnd
    , whereInValues
    , whereInSelect
    , whereBetween
    , whereExists
    
      -- ** GROUP BY
    , selectGroupBy
    , groupBySum
    , groupByAlias
    , groupByComplex
    , groupBySumHaving
    , havingComplex
    
      -- ** Functions
    , addition
    , multiplication
    , selectCurrentDate
    , selectRandom
    
      -- ** Combined queries
    , unionQuery
    , unionCombined
    
    -- * PostgreSQL
    , distinctOnSelect
    , fromLateral
    ) where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Database.Hedsql.SqLite

import qualified Database.Hedsql.Drivers.PostgreSQL.Constructor as P
import qualified Database.Hedsql.PostgreSQL                     as Pg

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

----------------------------------------
-- All Vendors
----------------------------------------

--------------------
-- SELECT
--------------------

-- | > SELECT * FROM "People"
selectAll :: Select a
selectAll = select (//*) /++ from "People"

-- | > SELECT DISTINCT "firstName" FROM "People"
distinctSelect :: Select a
distinctSelect = selectDistinct "firstName" /++ from "People"

--------------------
-- FROM
--------------------

-- Joins
--------------------

-- | > SELECT * FROM "People" CROSS JOIN "Countries"
fromCrossJoin :: Select a
fromCrossJoin =  select (//*) /++  from ("People" `crossJoin` "Countries")

{-|
> SELECT *
> FROM "People"
>   INNER JOIN "Countries" ON "People"."countryId" = "Countries"."countryId"
-}
fromInnerJoinOn :: Select a
fromInnerJoinOn =
        select (//*)
    /++ from (innerJoin "People" "Countries"
            $ "People" /. "countryId" /== "Countries" /. "countryId")

-- | > SELECT * FROM "People" INNER JOIN "Countries" USING ("country")
fromInnerJoinUsing :: Select a
fromInnerJoinUsing =
    select (//*) /++ from (innerJoin "People" "Countries" "countryId")

-- | > SELECT * FROM "People" NATURAL INNER JOIN "Countries"
fromNaturalInnerJoin :: Select a
fromNaturalInnerJoin =
    select (//*) /++ from ("People" `naturalInnerJoin` "Countries")

{-|
> SELECT * FROM "People" LEFT JOIN "Countries"
> ON "People"."countryId" = "Countries"."countryId"
-}
fromLeftJoinOn :: Select a
fromLeftJoinOn =
        select (//*)
    /++ from (leftJoin "People" "Countries"
            $ ("People"/."countryId") /== ("Countries"/."countryId")) 

-- | > SELECT * FROM "People" LEFT JOIN "Countries" USING ("countryId")
fromLeftJoinUsing :: Select a
fromLeftJoinUsing =
        select (//*)
    /++ from (leftJoin "People" "Countries" "countryId")

{-|
> SELECT * FROM "People" RIGHT JOIN "Countries"
> ON "People"."countryId" = "Countries"."countryId"
-}
fromRightJoinOn :: Select a
fromRightJoinOn =
        select (//*)
    /++ from (rightJoin "People" "Countries"
            $ ("People"/."countryId") /== ("Countries"/."countryId"))

{-|
> SELECT * FROM "People" FULL JOIN "Countries"
> ON "People"."countryId" = "Countries"."countryId"
-}
fromFullJoinOn :: Select a
fromFullJoinOn =
    select (//*) /++ from (fullJoin t1 t2 $ c1 /== c2)
    where
        t1 = table "People"
        t2 = table "Countries"
        c1 = t1/."countryId"
        c2 = t2/."countryId"

{-|
> SELECT * FROM "People" LEFT JOIN "Countries"
> ON ("People"."countryId" = "Countries"."countryId"
>      AND "Countries"."name" = 'Italy')
-}
fromLeftJoinOnAnd :: Select a
fromLeftJoinOnAnd =
        select (//*)
    /++ from (leftJoin t1 t2 cond)
    where
        t1 = table "People"
        t2 = table "Countries"
        cond = leftPart `and_` rightPart
        leftPart =  t1/."countryId" /== t2/."countryId"
        rightPart = t2/."name" /== value "Italy"

{-|
> SELECT *
> FROM "People" AS "Father"
>   INNER JOIN "People" AS "Child" ON "Father"."personId" = "Child"."father"
-}
selfJoin :: Select a
selfJoin =
    select (//*) /++ from (innerJoin father child cond)
    where
        cond = (father/."personId") /== (child/."father")
        father = people `alias` "Father"
        child = people `alias` "Child"
        people = table "People"

-- | > SELECT * FROM "People" AS "P" CROSS JOIN "Countries" AS "C"
crossJoinAlias :: Select a
crossJoinAlias =
        select (//*)
    /++ from (crossJoin ("People" `alias` "P") ("Countries" `alias` "C"))

-- | > SELECT * FROM ("People" AS "P" CROSS JOIN "Countries") AS "PC";
crossRefAlias :: Select a
crossRefAlias =
        select (//*)
    /++ from ((("People" `alias` "P") `crossJoin` "Countries") `alias` "PC")

-- Sub-queries
--------------------

-- | > SELECT * FROM (SELECT * FROM "People") AS "P";
selectSubQuery :: Select a
selectSubQuery =
    select (//*) /++ from (subQuery (select (//*) /++ from "People") "P")

--------------------
-- WHERE
--------------------

-- | > SELECT * FROM "People" AS "P" WHERE "P"."age" > 18;
whereAlias :: Select a
whereAlias =
        select (//*)
    /++ from p
    /++ where_ (p/."age" /> (5::Int))
    where
        p = table "People" `alias` "P"

{-|
> SELECT *
> FROM "People"
>   LEFT JOIN "Countries" ON "People"."countryId" = "Countries"."countryId"
> WHERE "Countries"."name" = 'Italy'
-}
leftJoinWhere :: Select a
leftJoinWhere =
        select (//*)
    /++ from (leftJoin people countries joinClause)
    /++ where_ (countries/."name" /== value "Italy")
    where
        joinClause = people/.countryId /== countries/.countryId
        countries = table "Countries"
        people = table "People"
        countryId = colRef "countryId"

{-|
@
SELECT *
FROM "People", "Countries"
WHERE ("People"."countryId" = "Country"."countryId" AND "People"."age" > 18)
@
-}
whereAnd :: Select a
whereAnd =
        select (//*)
    /++ from [people, countries]
    /++ where_ ((people/.id' /== countries/.id')
        `and_` (people/."age" /> (18::Int)))
    where
        people = tableRef "People"
        countries = tableRef "Countries"
        id' = colRef "countryId"

-- | > SELECT * FROM "Countries" WHERE "name" IN ('Italy', 'Switzerland')
whereInValues :: Select a
whereInValues =
        select (//*)
    /++ from "Countries"
    /++ where_ ("name" `in_` values ["Italy", "Switzerland"])

{-|
@
SELECT * FROM "People" WHERE "countryId" IN
    (SELECT "countryId" FROM "Countries" WHERE "inhabitants" >= ("size" * 100)))
@
-}
whereInSelect :: Select a
whereInSelect =
        select (//*)
    /++ from "People"
    /++ where_ ("countryId" `in_` query)
    where
        query =
                select "countryId" 
            /++ from "Countries"
            /++ where_ ("inhabitants" />= ("size" /* (100::Int)))

{-|
@
SELECT *
FROM "Countries"
WHERE ("inhabitants" BETWEEN (
    SELECT "age" FROM "People" WHERE "firstName" LIKE '*e*') AND 10000000)
@
-}
whereBetween :: Select a
whereBetween =
        select (//*)
    /++ from "Countries"
    /++ where_ (between "inhabitants" query (1000000::Int))
    where
        query =
                select "age"
            /++ from "People"
            /++ where_ ("firstName" `like` value "*e*")

{-|
@
SELECT *
FROM "People"
WHERE EXISTS (
    SELECT *
    FROM "Countries"
    WHERE "People"."countryId" = "Countries"."countryId")
@
-}
whereExists :: Select a
whereExists =
        select (//*)
    /++ from "People"
    /++ where_ (exists query)
    where
        query =
                select (//*)
            /++ from "Countries"
            /++ where_ ("People"/."countryId" /== "Countries"/."countryId")

--------------------
-- GROUP BY
--------------------

-- | > SELECT "age" FROM "People" GROUP BY "age"
selectGroupBy :: Select a
selectGroupBy = select "age" /++ from "People" /++ groupBy "age"

-- | > SELECT "lastName", sum("age") FROM "People" GROUP BY "lastName";
groupBySum :: Select a
groupBySum =
        select [lastName, colRef $ sum_ "age"]
    /++ from "People"
    /++ groupBy lastName
    where
        lastName = colRef "lastName"

-- | > SELECT "lastName" AS "name" FROM "People" GROUP BY "name"
groupByAlias :: Select a
groupByAlias =
        select name
    /++ from "People"
    /++ groupBy name
    where
        name = "lastName" `as_` "name"

{-|
@
SELECT
    "personId",
    "P"."lastName" AS "name",
    (SUM("C"."size") * "P"."age") AS "weirdFigure"
FROM "People" AS "P" LEFT JOIN "Countries" AS "C" USING ("personId")
GROUP BY "personId", "P"."name"
@
-}
groupByComplex :: Select a
groupByComplex =
         select [colRef personId, name, weird]
     /++ from (leftJoin people countries personId)
     /++ groupBy [colRef personId, name]
     where
         name = colRef (people/."lastName") `as_` "name"
         personId = toCol "personId"
         age = people/."age"
         weird = colRef (sum_ (countries/."size") /* age) `as_` "weirdFigure"
         people = table "People" `alias` "P"
         countries = table "Countries" `alias` "C"

{-|
> SELECT "lastName", SUM("age")
> FROM "People" GROUP BY "lastName" HAVING SUM("age") > 18
-}
groupBySumHaving :: Select a
groupBySumHaving =
        select [lastName, colRef sumAge]
    /++ from "People"
    /++ (groupBy lastName /++ having (sumAge /> (18::Int)))
    where
         lastName = colRef "lastName"
         sumAge = sum_ "age"

{-|
@
SELECT "personId", "P"."name", SUM("C"."size" * ("P"."age" - 2)) AS "weird"
FROM "People" AS "P" LEFT JOIN "Countries" AS "C" USING ("personId")
WHERE "personId" > 2
GROUP BY "personId", "P"."name", "P"."age"
HAVING SUM("P"."age" * "C"."size") > 5000000
@
-}
havingComplex :: Select a       
havingComplex =
         select [colRef personId, colRef name, weird]
     /++ from (leftJoin people countries personId)
     /++ where_ (personId /> (2::Int))
     /++ (   groupBy [colRef personId, name, age]
         /++ having (sum_ (age /* size) /> (5000000::Int))
         )
     where
         name = people/."name"
         personId = toCol "personId"
         age = people/."age"
         size = countries/."size"
         people = table "People" `alias` "P"
         countries = table "Countries" `alias`"C"
         weird = sum_ (size /* (age /- (2::Int))) `as_` "weird"

--------------------
-- Functions
--------------------

-- | > SELECT ("age" + 1) FROM "People"
addition :: Select a
addition = select (colRef $ "age" /+ (1::Int)) /++ from "People"
    
-- | > SELECT (3 * 4)
multiplication :: Select a
multiplication = select $ (3::Int) /* (4::Int)

{-|
MariaDB & PostgreSQL
> SELECT CURRENT_DATE

SqLite
> SELECT Date('now')
-}
selectCurrentDate :: Select a
selectCurrentDate = select currentDate

{-|
MariaDB
> SELECT RAND()

PostgreSQL & SqLite
> SELECT random()
-}
selectRandom :: Select a
selectRandom = select random

--------------------
-- Combined queries
--------------------

-- | Select a person by its primary key.
selectId :: Int -> Select a
selectId id' = select (//*) /++ from "People" /++ where_ ("personId" /== id') 

{-|
> (SELECT * FROM "People" WHERE "personId" = 1
> UNION SELECT * FROM "People" WHERE "personId" = 2)
-}
unionQuery :: CombinedQuery a        
unionQuery = union (selectId 1) (selectId 2)

{-|
> ((SELECT * FROM "People" WHERE "personId" = 1
> UNION SELECT * FROM "People" WHERE "personId" = 2)
> INTERSECT SELECT * FROM "People" WHERE "personId" = 1)
-}
unionCombined :: CombinedQuery a
unionCombined =
    intersect
        unionQuery
        (select (//*) /++ from "People" /++ where_ ("personId" /== (1::Int)))          

{-|
> (SELECT * FROM "People" WHERE "personId" = 1
> UNION ALL SELECT * FROM "People" WHERE "personId" = 2)
-}
unionAllQuery :: CombinedQuery a
unionAllQuery = unionAll (selectId 1) (selectId 2) 

----------------------------------------
-- PostgreSQL
----------------------------------------

-- | > SELECT DISTINCT ON ("firstName") * FROM "People" ORDER BY "age"
distinctOnSelect :: Select Pg.PostgreSQL
distinctOnSelect =
    P.selectDistinctOn "firstName" (//*) /++ from "People" /++ orderBy "age"
    
{-|
SELECT * FROM "Countries", LATERAL (
    SELECT *
    FROM "People"
    WHERE "People"."countryId" = "Countries"."countryId") AS "C"
-}
fromLateral :: Select Pg.PostgreSQL
fromLateral =
        select (//*)
    /++ from [tableRef "Countries", P.lateral subSelect "C"]
    where
        subSelect =
                select (//*)
            /++ from "People"
            /++ where_ (("People"/."countryId") /== ("Countries"/."countryId"))
    