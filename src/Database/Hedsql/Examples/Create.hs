{-|
Module      : Hedsql/Common/Parser/TableManipulations/Create.hs
Description : Collection of CREATE statements.
Copyright   : (c) Leonard Monnier, 2015
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

A collection of CREATE statements to be used in tests or as examples.
-}
module Database.Hedsql.Examples.Create
    (
    -- * Full examples
      countries
    , people
    
    -- * Basics
    , simpleTable
    , defaultVal
    
    -- * Constraints
    
    -- ** PRIMARY KEY
    , primaryKeyCol
    , primaryKeyColAuto
    , primaryKeyTable
    
    -- ** UNIQUE
    , createUnique
    , createUniqueT
    
    -- ** NOT NULL
    , noNulls
    
    -- ** FOREIGN KEY
    , createFK
    
    -- ** CHECK
    , createCheck
    , createChecks
    ) where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Database.Hedsql.SqLite

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

----------------------------------------
-- Full examples
----------------------------------------

{-|
MariaDB and SqLite:
@
CREATE TABLE "Countries" (
    "countryId"   INTEGER      PRIMARY KEY AUTOINCREMENT,
    "name"        VARCHAR(256) NOT NULL, UNIQUE,
    "size"        INTEGER,
    "inhabitants" INTEGER
)
@

PostgreSQL:
@
CREATE TABLE "Countries" (
    "countryId"   serial       PRIMARY KEY,
    "name"        varchar(256) NOT NUL, UNIQUE,
    "size"        integer,
    "inhabitants" integer
)
@
-}
countries :: Table a
countries =
    createTable
        "Countries"
        [ col "countryId"   integer       /++ primary True
        , col "name"        (varchar 256) /++ [notNull, unique]
        , col "size"        integer
        , col "inhabitants" integer
        ]

{-|
MariaDB and SqLite:
@
CREATE TABLE "People" (
   "personId"   INTEGER      PRIMARY KEY AUTOINCREMENT,
   "title"      CHAR(2)      DEFAULT('Ms')
   "firstName"  VARCHAR(256) NOT NULL,
   "lastName"   VARCHAR(256) NOT NULL,
   "age"        INTEGER      CHECK ("age" > -1),
   "father"     INTEGER      REFERENCES "People"("personId")
   "passportNo" VARCHAR(256) UNIQUE,
   "countryId"  INTEGER      REFERENCES "Countries"("countryId")
)
@

PostgreSQL:
@
CREATE TABLE "People" (
   "personId"   serial       PRIMARY KEY,
   "title"      char(2)      DEFAULT('Ms')
   "firstName"  varchar(256) NOT NULL,
   "lastName"   varchar(256) NOT NULL,
   "age"        integer      CHECK ("age" > -1),
   "passportNo" varchar(256) UNIQUE,
   "father"     integer      REFERENCES "People"("personId")
   "countryId"  integer      REFERENCES "Countries"("countryId")
)
@
-}
people :: Table a
people =
    createTable
        "People"
        [ col "personId"   integer       /++ primary True
        , col "title"      (char 2)      /++ defaultValue (value "Ms")
        , col "firstName"  (varchar 256) /++ notNull
        , col "lastName"   (varchar 256) /++ notNull
        , col "age"        integer       /++ check ("age" /> (-1::Int))
        , col "passportNo" (varchar 256) /++ unique
        , col "father"     integer       /++ foreignKey "People" "personId"
        , col "countryId"  integer       /++ foreignKey "Countries" "countryId"
        ]
----------------------------------------
-- Basics
----------------------------------------

-- | > CREATE TABLE "People" ("firstName" varchar(256))        
simpleTable :: Table a
simpleTable = createTable "People" [col "firstName" $ varchar 256]
      
-- | CREATE TABLE "People" ("country" integer DEFAULT(1))
defaultVal :: Table a
defaultVal = createTable
    "People"
    [col "country" integer /++ defaultValue (value (1::Int))]

----------------------------------------
-- Constraints
----------------------------------------

--------------------
-- PRIMARY KEY
--------------------

{-|
Maria DB and SqLite:
> CREATE TABLE "People" ("personId" INTEGER PRIMARY KEY)

PostgreSQL:
> CREATE TABLE "People" ("personId" integer PRIMARY KEY)
-}
primaryKeyCol :: Table a
primaryKeyCol =
    createTable "People" [col "personId" integer /++ primary False]

{-|
Maria DB and SqLite:
> CREATE TABLE "People" ("id" INTEGER PRIMARY KEY AUTOINCREMENT)

PostgreSQL:
> CREATE TABLE "People" ("id" serial PRIMARY KEY)
-}
primaryKeyColAuto :: Table a
primaryKeyColAuto =
    createTable "People" [col "personId" integer /++ primary True]

{-|
CREATE TABLE "People" (
    "firstName"       varchar(256),
    "lastName"        varchar(256),
    CONSTRAINT "pk" PRIMARY KEY ("firstName", "lastName")
)
-}
primaryKeyTable :: Table a
primaryKeyTable =
    createTable
        "People"
        (cols [("firstName", varchar 256), ("lastName", varchar 256)])
    /++ tableConstraint "pk" (primaryT ["firstName", "lastName"])

--------------------
-- UNIQUE
--------------------

-- | CREATE TABLE "People" ("passportNo" varchar(256) UNIQUE)
createUnique :: Table a
createUnique =
    createTable "People" [col "passportNo" (varchar 256) /++ unique]
    
{-|
CREATE TABLE "People" (
    "firstName" varchar(256),
    "lastName"  varchar(256),
    UNIQUE ("firstName", "lastName")
)
-}
createUniqueT :: Table a
createUniqueT =
    createTable "People" cs /++ tableConstraint "" (uniqueT cs)
    where
        cs =
            [ col "firstName" $ varchar 256
            , col "lastName"  $ varchar 256
            ]

--------------------
-- NOT NULL
--------------------

{-|
CREATE TABLE "People" (
    "firstName" varchar(256) CONSTRAINT "no_null" NOT NULL,
    "lastName"  varchar(256) NOT NULL
)
-}
noNulls :: Table a
noNulls =
    createTable "People" cs
    where
        cs =
            [ col "firstName" (varchar 256) /++ colConstraint "no_null" notNull
            , col "lastName"  (varchar 256) /++ notNull
            ]

--------------------
-- FOREIGN KEY
--------------------

{-|
CREATE TABLE "People" ("countryId" integer REFERENCES "Countries"("countryId"))
-}
createFK :: Table a
createFK =
    createTable
        "People"
        [col "countryId" integer /++ foreignKey "Countries" "countryId"]

--------------------
-- CHECK
--------------------
     
-- | CREATE TABLE "People" ("age" integer CHECK ("age" > -1))
createCheck :: Table a
createCheck =
    createTable
        "People"
        [col "age" integer /++ check ("age" /> (-1::Int))]
        
{-|            
CREATE TABLE "People" (
    "lastName" varchar(256),
    "age"      integer,
    CONSTRAINT "checks" CHECK ("age" > -1 AND "lastName" <> '')
)
-}
createChecks :: Table a
createChecks =
    createTable
        "People"
        (cols
        [ ("lastName", varchar 256)
        , ("age"     , integer    )
        ])
    /++ c1
    where
        c1 =
            tableConstraint "checks" $
                  checkT $ ("age" /> (-1::Int))
            `and_`("lastName"    /<> value "")