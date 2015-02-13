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
    
      -- ** Functions
    , addition
    , multiplication
    , selectCurrentDate
    , selectRandom
    
    -- * PostgreSQL
    , distinctOnSelect
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
>   JOIN "People" AS "Child" ON "Father"."personId" = "child"."father"
-}
selfTableJoin :: Select a
selfTableJoin =
    select (//*) /++ from (innerJoin father child cond)
    where
        cond = id /== fatherId
        child = people `alias` "child"
        father = people `alias` "father"
        people = table "People"
        id = people/."id"
        fatherId = "child"/."father"

{-|
> SELECT *
> FROM "my_table" AS "a" CROSS JOIN "my_table" AS "b"
-}
crossJoinAlias :: Select a
crossJoinAlias =
        select (//*)
    /++ from (crossJoin ("People" `alias` "P") ("Countries" `alias` "C"))

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

----------------------------------------
-- PostgreSQL
----------------------------------------

-- | > SELECT DISTINCT ON ("firstName") * FROM "People" ORDER BY "age"
distinctOnSelect :: Select Pg.PostgreSQL
distinctOnSelect =
    P.selectDistinctOn "firstName" (//*) /++ from "People" /++ orderBy "age"