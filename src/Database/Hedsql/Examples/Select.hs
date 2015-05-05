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
    
      -- ** ORDER BY
    , orderByQuery
    , orderBySum
    , orderByAscDesc
    , Database.Hedsql.Examples.Select.orderByLimit
    , orderByNull
    , Database.Hedsql.Examples.Select.orderByOffset
    
      -- ** GROUP BY
    , Database.Hedsql.Examples.Select.selectGroupBy
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
    , unionAllQuery
    , intersectAllQuery
    , exceptQuery
    , exceptAllQuery
    
    -- * PostgreSQL
    , distinctOnSelect
    , fromLateral
    ) where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Database.Hedsql.Ext
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
selectAll :: Select [[Undefined]] a
selectAll = select (//*) /++ from "People"

-- | > SELECT DISTINCT "firstName" FROM "People"
distinctSelect :: Select [Undefined] a
distinctSelect = selectDistinct "firstName" /++ from "People"

--------------------
-- FROM
--------------------

-- Joins
--------------------

-- | > SELECT * FROM "People" CROSS JOIN "Countries"
fromCrossJoin :: Select [[Undefined]] a
fromCrossJoin =  select (//*) /++  from ("People" `crossJoin` "Countries")

{-|
> SELECT *
> FROM "People"
>   INNER JOIN "Countries" ON "People"."countryId" = "Countries"."countryId"
-}
fromInnerJoinOn :: Select [[Undefined]] a
fromInnerJoinOn =
        select (//*)
    /++ from (innerJoin "People" "Countries"
            $ "People" /. "countryId" /== "Countries" /. "countryId")

-- | > SELECT * FROM "People" INNER JOIN "Countries" USING ("country")
fromInnerJoinUsing :: Select [[Undefined]] a
fromInnerJoinUsing =
    select (//*) /++ from (innerJoin "People" "Countries" "countryId")

-- | > SELECT * FROM "People" NATURAL INNER JOIN "Countries"
fromNaturalInnerJoin :: Select [[Undefined]] a
fromNaturalInnerJoin =
    select (//*) /++ from ("People" `naturalInnerJoin` "Countries")

{-|
> SELECT * FROM "People" LEFT JOIN "Countries"
> ON "People"."countryId" = "Countries"."countryId"
-}
fromLeftJoinOn :: Select [[Undefined]] a
fromLeftJoinOn =
        select (//*)
    /++ from (leftJoin "People" "Countries"
            $ "People"/."countryId" /== "Countries"/."countryId") 

-- | > SELECT * FROM "People" LEFT JOIN "Countries" USING ("countryId")
fromLeftJoinUsing :: Select [[Undefined]] a
fromLeftJoinUsing =
        select (//*)
    /++ from (leftJoin "People" "Countries" "countryId")

{-|
> SELECT * FROM "People" RIGHT JOIN "Countries"
> ON "People"."countryId" = "Countries"."countryId"
-}
fromRightJoinOn :: Select [[Undefined]] a
fromRightJoinOn =
        select (//*)
    /++ from (rightJoin "People" "Countries"
            $ "People"/."countryId" /== "Countries"/."countryId")

{-|
> SELECT * FROM "People" FULL JOIN "Countries"
> ON "People"."countryId" = "Countries"."countryId"
-}
fromFullJoinOn :: Select [[Undefined]] a
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
fromLeftJoinOnAnd :: Select [[Undefined]] a
fromLeftJoinOnAnd =
        select (//*)
    /++ from (leftJoin t1 t2 cond)
    where
        t1 = table "People"
        t2 = table "Countries"
        cond = leftPart `and_` rightPart
        leftPart =  t1/."countryId" /== t2/."countryId"
        rightPart = t2/. col "name" (varchar 256) /== value "Italy"

{-|
> SELECT *
> FROM "People" AS "Father"
>   INNER JOIN "People" AS "Child" ON "Father"."personId" = "Child"."father"
-}
selfJoin :: Select [[Undefined]] a
selfJoin =
    select (//*) /++ from (innerJoin father child cond)
    where
        cond = (father/."personId") /== (child/."father")
        father = people `alias` "Father"
        child = people `alias` "Child"
        people = table "People"

-- | > SELECT * FROM "People" AS "P" CROSS JOIN "Countries" AS "C"
crossJoinAlias :: Select [[Undefined]] a
crossJoinAlias =
        select (//*)
    /++ from (crossJoin ("People" `alias` "P") ("Countries" `alias` "C"))

-- | > SELECT * FROM ("People" AS "P" CROSS JOIN "Countries") AS "PC";
crossRefAlias :: Select [[Undefined]] a
crossRefAlias =
        select (//*)
    /++ from ((("People" `alias` "P") `crossJoin` "Countries") `alias` "PC")

-- Sub-queries
--------------------

-- | > SELECT * FROM (SELECT * FROM "People") AS "P";
selectSubQuery :: Select [[Undefined]] a
selectSubQuery =
    select (//*) /++ from (subQuery (select (//*) /++ from "People") "P")

--------------------
-- WHERE
--------------------

{-|
SELECT using a generic columns and values.

> SELECT "firstName" FROM "People" WHERE "age" > 18
-}
selectGen :: Select [Undefined] a
selectGen =
    select "firstName" /++ from "People" /++ where_ ("age" /> genVal (18::Int))

-- | > SELECT * FROM "People" AS "P" WHERE "P"."age" > 18;
whereAlias :: Select [[Undefined]] a
whereAlias =
        select (//*)
    /++ from p
    /++ where_ (p/. col "age" integer /> intVal 5)
    where
        p = table "People" `alias` "P"

{-|
> SELECT *
> FROM "People"
>   LEFT JOIN "Countries" ON "People"."countryId" = "Countries"."countryId"
> WHERE "Countries"."name" = 'Italy'
-}
leftJoinWhere :: Select [[Undefined]] a
leftJoinWhere =
        select (//*)
    /++ from (leftJoin people countries joinC)
    /++ where_ (countries/. col "name" (varchar 256) /== value "Italy")
    where
        joinC = people/.countryId /== countries/.countryId
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
whereAnd :: Select [[Undefined]] a
whereAnd =
        select (//*)
    /++ from [people, countries]
    /++ where_ ((people/.id' /== countries/.id')
        `and_` (people/. col "age" integer /> intVal 18))
    where
        people = tableRef "People"
        countries = tableRef "Countries"
        id' = colRef "countryId"

-- | > SELECT * FROM "Countries" WHERE "name" IN ('Italy', 'Switzerland')
whereInValues :: Select [[Undefined]] a
whereInValues =
        select (//*)
    /++ from "Countries"
    /++ where_ (col "name" (varchar 256) `in_` cs)
    where
        cs = colRef $ map genQVal ["Italy", "Switzerland"]

{-|
@
SELECT * FROM "People" WHERE "countryId" IN
    (SELECT "countryId" FROM "Countries" WHERE "inhabitants" >= "size" * 100))
@
-}
whereInSelect :: Select [[Undefined]] a
whereInSelect =
        select (//*)
    /++ from "People"
    /++ where_ (countryId `in_` query)
    where
        countryId = col "countryId" integer
        query =
                select countryId
            /++ from "Countries"
            /++ where_ (col "inhabitants" integer
                        />= (col "size" integer /* intVal 100))

{-|
@
SELECT *
FROM "Countries"
WHERE ("inhabitants" BETWEEN 10000 AND 10000000)
@
-}
whereBetween :: Select [[Undefined]] a
whereBetween =
        select (//*)
    /++ from "Countries"
    /++ where_ (between
        (col "inhabitants" integer)
        (intVal 10000)
        (intVal 1000000))

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
whereExists :: Select [[Undefined]] a
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
-- ORDER BY
--------------------

{-|
> SELECT "firstName", "lastName" FROM "People" ORDER BY "firstName, "lastName"
-}                        
orderByQuery :: Select [[String]] a
orderByQuery =
        select cs
    /++ from "People"
    /++ orderBy cs
    where
        cs =
            [ col "firstName" (varchar 256)
            , col "lastName" (varchar 256)
            ]

{-|
> SELECT "size" + "inhabitants" AS "sum", "name"
> FROM "Countries"
> ORDER BY "sum"
-}
orderBySum :: Select [[Undefined]] a
orderBySum =
        select [sum', colRefWrap "name"]
    /++ from "Countries"
    /++ orderBy sum'
    where
        sum' =
            wrap $ (col "size" integer /+ col "inhabitants" integer) `as_` "sum"

{-|
> SELECT "firstName", "lastName"
> FROM "People"
> ORDER BY "firstName" ASC, "lastName" DESC
-}
orderByAscDesc :: Select [[String]] a
orderByAscDesc =
        select [firstName, lastName]
    /++ from "People"
    /++ orderBy [asc firstName, desc lastName]
    where
        firstName = col "firstName" (varchar 256)
        lastName = col "lastName" (varchar 256)

{-|
> SELECT "age", "passeportNumber"
> FROM "People"
> ORDER BY "age" NULLS FIRST, "passeportNumber" NULLS LAST"
-}
orderByNull :: Select [[Int]] a
orderByNull =
        select [age, passeport]
    /++ from "People"
    /++ orderBy [nullsFirst age, nullsLast passeport]
    where
        age = col "age" integer
        passeport = col "passeportNumber" integer

-- | > SELECT * FROM "People" ORDER BY "firstName" LIMIT 2
orderByLimit :: Select [[Undefined]] a
orderByLimit =
        select (//*)
    /++ from "People"
    /++ (orderBy "firstName" /++ limit 2)

-- | > SELECT * FROM "People" ORDER BY "firstName" OFFSET 2
orderByOffset :: Select [[Undefined]] a
orderByOffset =
        select (//*)
    /++ from "People"
    /++ (orderBy "firstName" /++ offset 2)

--------------------
-- GROUP BY
--------------------

-- | > SELECT "age" FROM "People" GROUP BY "age"
selectGroupBy :: Select [Int] a
selectGroupBy =
        select age
    /++ from "People"
    /++ groupBy age
    where
        age = col "age" integer

-- | > SELECT "lastName", sum("age") FROM "People" GROUP BY "lastName";
groupBySum :: Select [[Undefined]] a
groupBySum =
        select [lastName, colRefWrap $ sum_ $ col "age" integer]
    /++ from "People"
    /++ groupBy lastName
    where
        lastName = colRefWrap $ col "lastName" (varchar 256)

-- | > SELECT "lastName" AS "name" FROM "People" GROUP BY "name"
groupByAlias :: Select [String] a
groupByAlias =
        select name
    /++ from "People"
    /++ groupBy name
    where
        name = col "lastName" (varchar 256) `as_` "name"

{-|
@
SELECT
    "personId",
    "P"."lastName" AS "name",
    SUM("C"."size") * "P"."age" AS "weirdFigure"
FROM "People" AS "P" LEFT JOIN "Countries" AS "C" USING ("personId")
GROUP BY "personId", "name"
@
-}
groupByComplex :: Select [[Undefined]] a
groupByComplex =
         select [colRefWrap personId, name, weird]
     /++ from (leftJoin people countries personId)
     /++ groupBy [colRefWrap personId, name]
     where
         name = colRefWrap $ (people/."lastName") `as_` "name"
         personId = toCol "personId"
         age = people /. col "age" integer
         weird = colRefWrap $ (sum_ (countries /. col "size" integer) /* age)
                    `as_` "weirdFigure"
         people = table "People" `alias` "P"
         countries = table "Countries" `alias` "C"

{-|
> SELECT "lastName", SUM("age")
> FROM "People" GROUP BY "lastName" HAVING SUM("age") > 18
-}
groupBySumHaving :: Select [[Undefined]] a
groupBySumHaving =
        select [lastName, colRefWrap sumAge]
    /++ from "People"
    /++ (groupBy lastName /++ having (sumAge /> intVal 18))
    where
         lastName = colRefWrap "lastName"
         sumAge = sum_ $ col "age" integer

{-|
@
SELECT "personId", "P"."name", SUM("C"."size" * ("P"."age" - 2)) AS "weird"
FROM "People" AS "P" LEFT JOIN "Countries" AS "C" USING ("personId")
WHERE "personId" > 2
GROUP BY "personId", "P"."name", "P"."age"
HAVING SUM("P"."age" * "C"."size") > 5000000
@
-}
havingComplex :: Select [[Undefined]] a       
havingComplex =
         select [colRefWrap personId, colRefWrap name, wrap weird]
     /++ from (leftJoin people countries personId)
     /++ where_ (personId /> intVal 2)
     /++ ( groupBy [colRefWrap personId, wrap name, wrap age]
     /++   having (sum_ (age /* size) /> intVal 5000000)
         )
     where
         name      = people/."name"
         personId  = col "personId" integer
         age       = people /. col "age" integer
         size      = countries /. col "size" integer
         people    = table "People" `alias` "P"
         countries = table "Countries" `alias`"C"
         weird     = sum_ (size /* (age /- intVal 2)) `as_` "weird"

--------------------
-- Comparison operators
--------------------

-- | Select all rows from the People table.
selectPeople :: Select [[Undefined]] a
selectPeople = select (//*) /++ from "People"

-- | > SELECT * FROM "People" WHERE "age" > 18
selectGreaterThan :: Select [[Undefined]] a
selectGreaterThan =
        selectPeople
    /++ where_ (col "age" integer /> intVal 18)

-- | > SELECT * FROM "People" WHERE "age" >= 18
selectGreaterThanOrEqualTo :: Select [[Undefined]] a
selectGreaterThanOrEqualTo =
        selectPeople
    /++ where_ (col "age" integer />= intVal 18)
    
-- | > SELECT * FROM "People" WHERE "age" < 18
selectSmallerThan :: Select [[Undefined]] a
selectSmallerThan =
        selectPeople
    /++ where_ (col "age" integer /< intVal 18)
    
-- | > SELECT * FROM "People" WHERE "age" <= 18
selectSmallerThanOrEqualTo :: Select [[Undefined]] a
selectSmallerThanOrEqualTo =
        selectPeople
    /++ where_ (col "age" integer /<= intVal 18)
    
-- | > SELECT * FROM "People" WHERE "age" = 18
selectEqual :: Select [[Undefined]] a
selectEqual = selectPeople /++ where_ (col "age" integer /== intVal 18)
    
-- | > SELECT * FROM "People" WHERE "age" <> 18
selectNotEqual :: Select [[Undefined]] a
selectNotEqual = selectPeople /++ where_ (col "age" integer /<> intVal 18)
    
-- | > SELECT * FROM "People" WHERE ("age" NOT BETWEEN 5 AND 18)
selectNotBetween :: Select [[Undefined]] a
selectNotBetween =
        selectPeople
    /++ where_
            (notBetween (col "age" integer) (intVal 5) (intVal 18))

--------------------
-- Boolean operators
--------------------

-- | > SELECT * FROM "People" WHERE "passeportNumber" IS NULL
isNullQuery :: Select [[Undefined]] a
isNullQuery = selectPeople /++ where_ (isNull "passeportNumber")
    
-- | > SELECT * FROM "People" WHERE "passeportNumber" IS NOT NULL
isNotNullQuery :: Select [[Undefined]] a
isNotNullQuery = selectPeople /++ where_ (isNotNull "passeportNumber")
    
{-|
> SELECT *
> FROM "People" WHERE "nickNameAsKind" IS DISTINCT FROM "nickNameAsAdult"
-}
isDistinctFromQuery :: Select [[Undefined]] a
isDistinctFromQuery =
        selectPeople
    /++ where_ ("nickNameAsKind" `isDistinctFrom` "nickNameAsAdult")

{-|
> SELECT *
> FROM "People" WHERE "nickNameAsKind" IS NOT DISTINCT FROM "nickNameAsAdult"
-}
isNotDistinctFromQuery :: Select [[Undefined]] a
isNotDistinctFromQuery =
        selectPeople
    /++ where_ ("nickNameAsKind" `isNotDistinctFrom` "nickNameAsAdult")

-- | > SELECT * FROM "People" WHERE "married" IS TRUE
isTrueQuery :: Select [[Undefined]] a
isTrueQuery = selectPeople /++ where_ (isTrue $ col "married" boolean)
    
-- | > SELECT * FROM "People" WHERE "married" IS NOT TRUE
isNotTrueQuery :: Select [[Undefined]] a
isNotTrueQuery = selectPeople /++ where_ (isNotTrue $ col "married" boolean)
    
-- | > SELECT * FROM "People" WHERE "married" IS FALSE
isFalseQuery :: Select [[Undefined]] a
isFalseQuery = selectPeople /++ where_ (isFalse $ col "married" boolean)
    
-- | > SELECT * FROM "People" WHERE "married" IS NOT FALSE
isNotFalseQuery :: Select [[Undefined]] a
isNotFalseQuery = selectPeople /++ where_ (isNotFalse $ col "married" boolean)
    
{-|
> SELECT * FROM "People"
> WHERE ("nickNameAsKind" = "nickNameAsAdult") IS UNKWNOWN
-}
isUnknownQuery :: Select [[Undefined]] a
isUnknownQuery =
    selectPeople /++ where_ (isUnknown $ "nickNameAsKind" /== "nickNameAsAdult")
    
{-|
> SELECT *
> FROM "People" WHERE ("nickNameAsKind" = "nickNameAsAdult") IS NOT UNKWNOWN
-}
isNotUnknownQuery :: Select [[Undefined]] a
isNotUnknownQuery =
        selectPeople
    /++ where_ (isNotUnknown $ "nickNameAsKind" /== "nickNameAsAdult")

--------------------
-- Functions
--------------------

-- | > SELECT "age" + 1 FROM "People"
addition :: Select [Int] a
addition =
        select (col "age" integer /+ intVal 1)
    /++ from "People"
    
-- | > SELECT 3 * 4
multiplication :: Select [Int] a
multiplication = select $ intVal 3 /* intVal 4

{-|
MariaDB & PostgreSQL
> SELECT CURRENT_DATE

SqLite
> SELECT Date('now')
-}
selectCurrentDate :: Select [Time] a
selectCurrentDate = select currentDate

{-|
MariaDB
> SELECT RAND()

PostgreSQL & SqLite
> SELECT random()
-}
selectRandom :: Select [Int] a
selectRandom = select random

--------------------
-- Combined queries
--------------------

-- | Select a person by its primary key.
selectId :: Int -> Select [[Undefined]] a
selectId id' =
        select (//*)
    /++ from "People"
    /++ where_ (col "personId" integer /== value id') 

{-|
> SELECT * FROM "People" WHERE "personId" = 1
> UNION
> SELECT * FROM "People" WHERE "personId" = 2
-}
unionQuery :: Select [[Undefined]] a        
unionQuery = union (selectId 1) $ selectId 2

{-|
> (SELECT * FROM "People" WHERE "personId" = 1
> UNION
> SELECT * FROM "People" WHERE "personId" = 2)
> INTERSECT
> SELECT * FROM "People" WHERE "personId" = 1
-}
unionCombined :: Select [[Undefined]] a
unionCombined =
    intersect
        unionQuery
        (select (//*)
            /++ from "People"
            /++ where_ (col "personId" integer /== intVal 1))          

{-|
> SELECT * FROM "People" WHERE "personId" = 1
> UNION ALL
> SELECT * FROM "People" WHERE "personId" = 2
-}
unionAllQuery :: Select [[Undefined]] a
unionAllQuery = unionAll (selectId 1) $ selectId 2 

{-|
> SELECT * FROM "People" WHERE "personId" = 1
> INTERSECT ALL
> SELECT * FROM "People" WHERE "personId" = 2
-}
intersectAllQuery :: Select [[Undefined]] a
intersectAllQuery = intersectAll (selectId 1) $ selectId 2

{-|
> SELECT * FROM "People"
> EXCEPT
> SELECT * FROM "People" WHERE "personId" = 1
-}
exceptQuery :: Select [[Undefined]] a
exceptQuery = except (select (//*) /++ from "People") $ selectId 1

{-|
> SELECT * FROM "People"
> EXCEPT ALL
> SELECT * FROM "People" WHERE "personId" = 1
-}
exceptAllQuery :: Select [[Undefined]] a
exceptAllQuery =
    exceptAll (select (//*) /++ from "People") $ selectId 1

----------------------------------------
-- PostgreSQL
----------------------------------------

-- | > SELECT DISTINCT ON ("firstName") * FROM "People" ORDER BY "age"
distinctOnSelect :: Select [[Undefined]] Pg.PostgreSQL
distinctOnSelect =
        P.selectDistinctOn [colRefWrap "firstName"] (//*)
    /++ from "People"
    /++ orderBy "age"
    
{-|
SELECT * FROM "Countries", LATERAL (
    SELECT *
    FROM "People"
    WHERE "People"."countryId" = "Countries"."countryId") AS "C"
-}
fromLateral :: Select [[Undefined]] Pg.PostgreSQL
fromLateral =
        select (//*)
    /++ from [tableRef "Countries", P.lateral (wrap subSelect) "C"]
    where
        subSelect =
                select (//*)
            /++ from "People"
            /++ where_ ("People"/."countryId" /== "Countries"/."countryId")
    