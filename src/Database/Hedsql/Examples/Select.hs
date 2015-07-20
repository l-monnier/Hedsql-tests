{-|
Module      : Hedsql/Common/Parser/Queries/Query.hs
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
    , selectTwoCols
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
    , nestedJoins

      -- *** Sub-queries
    , selectSubQuery

      -- ** WHERE
    , selectGen
    , whereAlias
    , whereAnd
    , whereAnds
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
    , orderByLimitOffset

      -- ** GROUP BY
    , Database.Hedsql.Examples.Select.selectGroupBy
    , groupByTwo
    , groupBySum
    , groupByAlias
    , groupByComplex
    , groupBySumHaving
    , groupBySumHavingTwo
    , havingComplex

      -- ** Full
    , selectFull

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
selectAll :: Query [[Undefined]] a
selectAll = do
    select (//*)
    from "People"

{-|
@
SELECT
  "firstName",
  "lastName"
FROM "People"
@
-}
selectTwoCols :: Query [[Undefined]] a
selectTwoCols = do
    select ["firstName", "lastName"]
    from "People"

-- | > SELECT DISTINCT "firstName" FROM "People"
distinctSelect :: Query [Undefined] a
distinctSelect = do
    selectDistinct "firstName"
    from "People"

--------------------
-- FROM
--------------------

-- Joins
--------------------

-- | > SELECT * FROM "People" CROSS JOIN "Countries"
fromCrossJoin :: Query [[Undefined]] a
fromCrossJoin = do
    select (//*)
    from $ "People" `crossJoin` "Countries"

{-|
@
SELECT *
FROM "People"
INNER JOIN "Countries"
ON "People"."countryId" = "Countries"."countryId"
@
-}
fromInnerJoinOn :: Query [[Undefined]] a
fromInnerJoinOn = do
    select (//*)
    from $ innerJoin "People" "Countries" $ "People" /. "countryId" /== "Countries" /. "countryId"

-- | > SELECT * FROM "People" INNER JOIN "Countries" USING ("country")
fromInnerJoinUsing :: Query [[Undefined]] a
fromInnerJoinUsing = do
    select (//*)
    from $ innerJoin "People" "Countries" "countryId"

-- | > SELECT * FROM "People" NATURAL INNER JOIN "Countries"
fromNaturalInnerJoin :: Query [[Undefined]] a
fromNaturalInnerJoin = do
    select (//*)
    from $ "People" `naturalInnerJoin` "Countries"

{-|
> SELECT * FROM "People" LEFT JOIN "Countries"
> ON "People"."countryId" = "Countries"."countryId"
-}
fromLeftJoinOn :: Query [[Undefined]] a
fromLeftJoinOn = do
    select (//*)
    from $ leftJoin "People" "Countries" $ "People"/."countryId" /== "Countries"/."countryId"

-- | > SELECT * FROM "People" LEFT JOIN "Countries" USING ("countryId")
fromLeftJoinUsing :: Query [[Undefined]] a
fromLeftJoinUsing = do
    select (//*)
    from $ leftJoin "People" "Countries" "countryId"

{-|
> SELECT * FROM "People" RIGHT JOIN "Countries"
> ON "People"."countryId" = "Countries"."countryId"
-}
fromRightJoinOn :: Query [[Undefined]] a
fromRightJoinOn = do
    select (//*)
    from $ rightJoin "People" "Countries" $ "People"/."countryId" /== "Countries"/."countryId"

{-|
> SELECT * FROM "People" FULL JOIN "Countries"
> ON "People"."countryId" = "Countries"."countryId"
-}
fromFullJoinOn :: Query [[Undefined]] a
fromFullJoinOn = do
    select (//*)
    from $ fullJoin t1 t2 $ c1 /== c2
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
fromLeftJoinOnAnd :: Query [[Undefined]] a
fromLeftJoinOnAnd = do
    select (//*)
    from $ leftJoin t1 t2 cond
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
selfJoin :: Query [[Undefined]] a
selfJoin = do
    select (//*)
    from $ innerJoin father child cond
    where
        cond = (father/."personId") /== (child/."father")
        father = people `alias` "Father"
        child = people `alias` "Child"
        people = table "People"

-- | > SELECT * FROM "People" AS "P" CROSS JOIN "Countries" AS "C"
crossJoinAlias :: Query [[Undefined]] a
crossJoinAlias = do
        select (//*)
        from $ crossJoin ("People" `alias` "P") ("Countries" `alias` "C")

-- | > SELECT * FROM ("People" AS "P" CROSS JOIN "Countries") AS "PC";
crossRefAlias :: Query [[Undefined]] a
crossRefAlias = do
        select (//*)
        from $ (("People" `alias` "P") `crossJoin` "Countries") `alias` "PC"

{-|
@
SELECT *
FROM "People"
INNER JOIN "Countries"
ON "People"."countryId" = "Countries"."countryId"
INNER JOIN "Addresses"
ON "People"."personId" = "Addresses"."personId"
@
-}
nestedJoins :: Query [[Undefined]] a
nestedJoins = do
    select (//*)
    from join2
    where
        join1 = innerJoin "People" "Countries" $ "People" /. "countryId" /== "Countries" /. "countryId"
        join2 = innerJoin join1 "Addresses" $ "People" /. "personId" /== "Addresses" /. "personId"

-- Sub-queries
--------------------

{-|
@
SELECT *
FROM (SELECT *
      FROM "People") AS "P"
@
-}
selectSubQuery :: Query [[Undefined]] a
selectSubQuery = do
    select (//*)
    from (subQuery (do
        select (//*)
        from "People") "P")

--------------------
-- WHERE
--------------------

{-|
SELECT using a generic columns and values.

> SELECT "firstName" FROM "People" WHERE "age" > 18
-}
selectGen :: Query [Undefined] a
selectGen = do
    select "firstName"
    from "People"
    where_ $ "age" /> genVal (18::Int)

-- | > SELECT * FROM "People" AS "P" WHERE "P"."age" > 18;
whereAlias :: Query [[Undefined]] a
whereAlias = do
    select (//*)
    from p
    where_ $ p/. col "age" integer /> intVal 5
    where
        p = table "People" `alias` "P"

{-|
@
SELECT *
FROM
  "People",
  "Countries"
WHERE
  "People"."countryId" = "Countries"."countryId"
  AND "People"."age" > 18
@
-}
whereAnd :: Query [[Undefined]] a
whereAnd = do
    select (//*)
    from [people, countries]
    where_ $ (people/.id' /== countries/.id')
        `and_` (people/. col "age" integer /> intVal 18)
    where
        people = tableRef "People"
        countries = tableRef "Countries"
        id' = colRef "countryId"

{-|
@
SELECT *
FROM
  "People",
  "Countries"
WHERE
  "People"."countryId" = "Countries"."countryId"
  AND "People"."age" > 18
  AND "People"."age" < 70
@
-}
whereAnds :: Query [[Undefined]] a
whereAnds = do
    select (//*)
    from [people, countries]
    where_ $ (people/.id' /== countries/.id')
        `and_` (people/. age /> intVal 18)
        `and_` (people/. age /< intVal 70)
    where
        people = tableRef "People"
        countries = tableRef "Countries"
        id' = colRef "countryId"
        age = col "age" integer

-- | > SELECT * FROM "Countries" WHERE "name" IN ('Italy', 'Switzerland')
whereInValues :: Query [[Undefined]] a
whereInValues = do
    select (//*)
    from "Countries"
    where_ $ col "name" (varchar 256) `in_` cs
    where
        cs = colRef $ map genQVal ["Italy", "Switzerland"]

{-|
@
SELECT *
FROM "People"
WHERE "countryId" IN (SELECT "countryId"
                      FROM "Countries"
                      WHERE "inhabitants" >= "size" * 100)
@
-}
whereInSelect :: Query [[Undefined]] a
whereInSelect = do
    select (//*)
    from "People"
    where_ $ countryId `in_` query
    where
        countryId = col "countryId" integer
        query = do
            select countryId
            from "Countries"
            where_ $ col "inhabitants" integer />= (col "size" integer /* intVal 100)

{-|
@
SELECT *
FROM "Countries"
WHERE ("inhabitants" BETWEEN 10000 AND 10000000)
@
-}
whereBetween :: Query [[Undefined]] a
whereBetween = do
    select (//*)
    from "Countries"
    where_ $ between (col "inhabitants" integer) (intVal 10000) (intVal 1000000)

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
whereExists :: Query [[Undefined]] a
whereExists = do
    select (//*)
    from "People"
    where_ $ exists query
    where
        query = do
            select (//*)
            from "Countries"
            where_ ("People"/."countryId" /== "Countries"/."countryId")

--------------------
-- ORDER BY
--------------------

{-|
> SELECT "firstName" FROM "People" ORDER BY "firstName
-}
orderByQuery :: Query [String] a
orderByQuery = do
    select c
    from "People"
    orderBy c
    where
        c = col "firstName" (varchar 256)

{-|
> SELECT "size" + "inhabitants" AS "sum", "name"
> FROM "Countries"
> ORDER BY "sum"
-}
orderBySum :: Query [[Undefined]] a
orderBySum = do
    select [sum', colRefWrap "name"]
    from "Countries"
    orderBy sum'
    where
        sum' =
            wrap $ (col "size" integer /+ col "inhabitants" integer) `as_` "sum"

{-|
@
SELECT
  "firstName",
  "lastName"
FROM "People"
ORDER BY
  "firstName" ASC,
  "lastName" DESC
@
-}
orderByAscDesc :: Query [[String]] a
orderByAscDesc = do
    select [firstName, lastName]
    from "People"
    orderBy [asc firstName, desc lastName]
    where
        firstName = col "firstName" (varchar 256)
        lastName = col "lastName" (varchar 256)

{-|
> SELECT "age", "passeportNumber"
> FROM "People"
> ORDER BY "age" NULLS FIRST, "passeportNumber" NULLS LAST"
-}
orderByNull :: Query [[Int]] a
orderByNull = do
    select [age, passeport]
    from "People"
    orderBy [nullsFirst age, nullsLast passeport]
    where
        age = col "age" integer
        passeport = col "passeportNumber" integer

{-|
@
SELECT *
FROM "People"
ORDER BY "firstName"
LIMIT 2
@
-}
orderByLimit :: Query [[Undefined]] a
orderByLimit = do
    select (//*)
    from "People"
    orderBy "firstName"
    limit 2

{-|
@
SELECT *
FROM "People"
ORDER BY "firstName"
OFFSET 2
@
-}
orderByOffset :: Query [[Undefined]] a
orderByOffset = do
    select (//*)
    from "People"
    orderBy "firstName"
    offset 2

{-|
@
SELECT *
FROM "People"
ORDER BY "firstName"
LIMIT 5 OFFSET 2
@
-}
orderByLimitOffset :: Query [[Undefined]] a
orderByLimitOffset = do
    select (//*)
    from "People"
    orderBy "firstName"
    limit 5
    offset 2


--------------------
-- GROUP BY
--------------------

{-|
@
SELECT "age"
FROM "People"
GROUP BY "age"
@
-}
selectGroupBy :: Query [Int] a
selectGroupBy = do
    select age
    from "People"
    groupBy age
    where
        age = col "age" integer

{-|
@
SELECT
  "firstName",
  "age"
FROM "People"
GROUP BY
  "firstName",
  "age"
@
-}
groupByTwo :: Query [[Undefined]] a
groupByTwo = do
    select cs
    from "People"
    groupBy cs
    where
        cs = ["firstName", "age"]

-- | > SELECT "lastName", sum("age") FROM "People" GROUP BY "lastName";
groupBySum :: Query [[Undefined]] a
groupBySum = do
    select [lastName, colRefWrap $ sum_ $ col "age" integer]
    from "People"
    groupBy lastName
    where
        lastName = colRefWrap $ col "lastName" (varchar 256)

-- | > SELECT "lastName" AS "name" FROM "People" GROUP BY "name"
groupByAlias :: Query [String] a
groupByAlias = do
    select name
    from "People"
    groupBy name
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
groupByComplex :: Query [[Undefined]] a
groupByComplex = do
     select [colRefWrap personId, name, weird]
     from (leftJoin people countries personId)
     groupBy [colRefWrap personId, name]
     where
         name = colRefWrap $ (people/."lastName") `as_` "name"
         personId = toCol "personId"
         age = people /. col "age" integer
         weird = colRefWrap $ (sum_ (countries /. col "size" integer) /* age)
                    `as_` "weirdFigure"
         people = table "People" `alias` "P"
         countries = table "Countries" `alias` "C"

{-|
@
SELECT
  "lastName",
  SUM("age")
FROM "People"
GROUP BY "lastName"
HAVING SUM("age") > 18
@
-}
groupBySumHaving :: Query [[Undefined]] a
groupBySumHaving = do
    select [lastName, colRefWrap sumAge]
    from "People"
    groupBy lastName
    having $ sumAge /> intVal 18
    where
         lastName = colRefWrap "lastName"
         sumAge = sum_ $ col "age" integer

{-|
@
SELECT "firstName"
FROM "People"
GROUP BY "firstName"
HAVING
  sum("age") > 18
  OR sum("size") < 1800
@
-}
groupBySumHavingTwo :: Query [Undefined] a
groupBySumHavingTwo = do
    select "firstName"
    from "People"
    groupBy "firstName"
    having ((sumAge /> intVal 18) `or_` (sumSize /< intVal 1800))
    where
        sumAge = sum_ $ col "age" integer
        sumSize = sum_ $ col "size" integer

{-|
@
SELECT "personId", "P"."name", SUM("C"."size" * ("P"."age" - 2)) AS "weird"
FROM "People" AS "P" LEFT JOIN "Countries" AS "C" USING ("personId")
WHERE "personId" > 2
GROUP BY "personId", "P"."name", "P"."age"
HAVING SUM("P"."age" * "C"."size") > 5000000
@
-}
havingComplex :: Query [[Undefined]] a
havingComplex = do
     select [colRefWrap personId, colRefWrap name, wrap weird]
     from $ leftJoin people countries personId
     where_ $ personId /> intVal 2
     groupBy [colRefWrap personId, wrap name, wrap age]
     having $ sum_ (age /* size) /> intVal 5000000
     where
         name      = people/."name"
         personId  = col "personId" integer
         age       = people /. col "age" integer
         size      = countries /. col "size" integer
         people    = table "People" `alias` "P"
         countries = table "Countries" `alias`"C"
         weird     = sum_ (size /* (age /- intVal 2)) `as_` "weird"

--------------------
-- Full
--------------------

{-|
@
SELECT *
FROM "People"
WHERE "age" > 18
GROUPBY "lastName"
HAVING SUM("age") > 100
ORDER BY "id"
LIMIT 30 OFFSET 2
@
-}
selectFull :: Query [[Undefined]] a
selectFull = do
    select (//*)
    from $ table "People"
    where_ (age /> intVal 18)
    groupBy $ col "lastName" $ varchar 256
    having (sum_ age /> intVal 100)
    orderBy $ col "id" integer
    limit 30
    offset 2
    where
        age = col "age" integer

--------------------
-- Comparison operators
--------------------

-- | Query all rows from the People table.
selectPeople :: Query [[Undefined]] a
selectPeople = do
    select (//*)
    from "People"

-- | > SELECT * FROM "People" WHERE "age" > 18
selectGreaterThan :: Query [[Undefined]] a
selectGreaterThan = do
    selectPeople
    where_ $ col "age" integer /> intVal 18

-- | > SELECT * FROM "People" WHERE "age" >= 18
selectGreaterThanOrEqualTo :: Query [[Undefined]] a
selectGreaterThanOrEqualTo = do
    selectPeople
    where_ $ col "age" integer />= intVal 18

-- | > SELECT * FROM "People" WHERE "age" < 18
selectSmallerThan :: Query [[Undefined]] a
selectSmallerThan = do
    selectPeople
    where_ $ col "age" integer /< intVal 18

-- | > SELECT * FROM "People" WHERE "age" <= 18
selectSmallerThanOrEqualTo :: Query [[Undefined]] a
selectSmallerThanOrEqualTo = do
    selectPeople
    where_ $ col "age" integer /<= intVal 18

-- | > SELECT * FROM "People" WHERE "age" = 18
selectEqual :: Query [[Undefined]] a
selectEqual = do
    selectPeople
    where_ $ col "age" integer /== intVal 18

-- | > SELECT * FROM "People" WHERE "age" <> 18
selectNotEqual :: Query [[Undefined]] a
selectNotEqual = do
    selectPeople
    where_ $ col "age" integer /<> intVal 18

-- | > SELECT * FROM "People" WHERE ("age" NOT BETWEEN 5 AND 18)
selectNotBetween :: Query [[Undefined]] a
selectNotBetween = do
    selectPeople
    where_ $ notBetween (col "age" integer) (intVal 5) $ intVal 18

--------------------
-- Boolean operators
--------------------

-- | > SELECT * FROM "People" WHERE "passeportNumber" IS NULL
isNullQuery :: Query [[Undefined]] a
isNullQuery = do
    selectPeople
    where_ $ isNull "passeportNumber"

-- | > SELECT * FROM "People" WHERE "passeportNumber" IS NOT NULL
isNotNullQuery :: Query [[Undefined]] a
isNotNullQuery = do
    selectPeople
    where_ $ isNotNull "passeportNumber"

{-|
> SELECT *
> FROM "People" WHERE "nickNameAsKind" IS DISTINCT FROM "nickNameAsAdult"
-}
isDistinctFromQuery :: Query [[Undefined]] a
isDistinctFromQuery = do
    selectPeople
    where_ $ "nickNameAsKind" `isDistinctFrom` "nickNameAsAdult"

{-|
> SELECT *
> FROM "People" WHERE "nickNameAsKind" IS NOT DISTINCT FROM "nickNameAsAdult"
-}
isNotDistinctFromQuery :: Query [[Undefined]] a
isNotDistinctFromQuery = do
    selectPeople
    where_ $ "nickNameAsKind" `isNotDistinctFrom` "nickNameAsAdult"

-- | > SELECT * FROM "People" WHERE "married" IS TRUE
isTrueQuery :: Query [[Undefined]] a
isTrueQuery = do
    selectPeople
    where_ $ isTrue $ col "married" boolean

-- | > SELECT * FROM "People" WHERE "married" IS NOT TRUE
isNotTrueQuery :: Query [[Undefined]] a
isNotTrueQuery = do
    selectPeople
    where_ $ isNotTrue $ col "married" boolean

-- | > SELECT * FROM "People" WHERE "married" IS FALSE
isFalseQuery :: Query [[Undefined]] a
isFalseQuery = do
    selectPeople
    where_ $ isFalse $ col "married" boolean

-- | > SELECT * FROM "People" WHERE "married" IS NOT FALSE
isNotFalseQuery :: Query [[Undefined]] a
isNotFalseQuery = do
    selectPeople
    where_ $ isNotFalse $ col "married" boolean

{-|
> SELECT * FROM "People"
> WHERE ("nickNameAsKind" = "nickNameAsAdult") IS UNKWNOWN
-}
isUnknownQuery :: Query [[Undefined]] a
isUnknownQuery = do
    selectPeople
    where_ $ isUnknown $ "nickNameAsKind" /== "nickNameAsAdult"

{-|
> SELECT *
> FROM "People" WHERE ("nickNameAsKind" = "nickNameAsAdult") IS NOT UNKWNOWN
-}
isNotUnknownQuery :: Query [[Undefined]] a
isNotUnknownQuery = do
    selectPeople
    where_ $ isNotUnknown $ "nickNameAsKind" /== "nickNameAsAdult"

--------------------
-- Functions
--------------------

-- | > SELECT "age" + 1 FROM "People"
addition :: Query [Int] a
addition = do
    select $ col "age" integer /+ intVal 1
    from "People"

-- | > SELECT 3 * 4
multiplication :: Query [Int] a
multiplication = do
    select $ intVal 3 /* intVal 4

{-|
MariaDB & PostgreSQL
> SELECT CURRENT_DATE

SqLite
> SELECT Date('now')
-}
selectCurrentDate :: Query [Time] a
selectCurrentDate = do
    select currentDate

{-|
MariaDB
> SELECT RAND()

PostgreSQL & SqLite
> SELECT random()
-}
selectRandom :: Query [Int] a
selectRandom = do
    select random

--------------------
-- Combined queries
--------------------

-- | Query a person by its primary key.
selectId :: Int -> Query [[Undefined]] a
selectId id' = do
    select (//*)
    from "People"
    where_ $ col "personId" integer /== value id'

{-|
@
SELECT *
FROM "People"
WHERE "personId" = 1
UNION
SELECT *
FROM "People"
WHERE "personId" = 2
@
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
        unionQuery (do
            select (//*)
            from "People"
            where_ (col "personId" integer /== intVal 1))

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
exceptQuery = except (do
    select (//*)
    from "People") (selectId 1)

{-|
> SELECT * FROM "People"
> EXCEPT ALL
> SELECT * FROM "People" WHERE "personId" = 1
-}
exceptAllQuery :: Select [[Undefined]] a
exceptAllQuery =
    exceptAll (do
        select (//*)
        from "People") (selectId 1)

----------------------------------------
-- PostgreSQL
----------------------------------------

-- | > SELECT DISTINCT ON ("firstName") * FROM "People" ORDER BY "age"
distinctOnSelect :: Query [[Undefined]] Pg.PostgreSQL
distinctOnSelect = do
    P.selectDistinctOn [colRefWrap "firstName"] (//*)
    from "People"
    orderBy "age"

{-|
SELECT * FROM "Countries", LATERAL (
    SELECT *
    FROM "People"
    WHERE "People"."countryId" = "Countries"."countryId") AS "C"
-}
fromLateral :: Query [[Undefined]] Pg.PostgreSQL
fromLateral = do
    select (//*)
    from [tableRef "Countries", P.lateral (wrap subQuery) "C"]
    where
        subQuery = do
            select (//*)
            from "People"
            where_ $ "People"/."countryId" /== "Countries"/."countryId"

----------------------------------------
-- Quick start tutorial
----------------------------------------

myQueryOne :: Query [[Undefined]] a
myQueryOne = do
    select (//*)
    from $ table "films"

myQueryTwo :: Query [[Int]] SqLite
myQueryTwo = do
    select [id', age]
    from films
    where_ $ age /+ intVal 20 `in_` subSelect
    where
        id' = col "id" integer
        age = col "age" integer
        films = table "films"
        actor = table "actors"
        subSelect = do
            select age
            from actor

--myQueryTwo' :: Query [[Undefined]] SqLite
--myQueryTwo' =
--        select [wrap id', wrap age]
--    /++ from films
--    /++ where_ (age /+ 20 `in_` select age /++ from actor)
--    where
--        id' = col "id" integer
--        age = col "age" $ varchar 256
--        films = table "films"
--        actor = table "actors"

myQueryThree :: Query [[Undefined]] Pg.PostgreSQL
myQueryThree = do
        select (//*)
        from [tableRef $ table "foo", Pg.lateral (wrap sub) "s"]
        where
            sub = do
                select (//*)
                from "bar"

myQueryFour :: Query [[Undefined]] a
myQueryFour = do
    select ["firstName", "lastName"]
    from "People"
    where_ $ "age" /> genVal (18::Int)

myQueryFive :: Query [[Undefined]] a
myQueryFive = do
    select [wrap name, wrap age]
    from $ table "People"
    where
        name = col "name" $ varchar 256
        age = col "age" integer
