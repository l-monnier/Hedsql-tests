{-|
Module      : Database/Hedsql/Examples/Insert.hs
Description : Collection of INSERT statements.
Copyright   : (c) Leonard Monnier, 2015
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

A collection of INSERT statements to be used in tests or as examples.
-}
module Database.Hedsql.Examples.Insert
    (
      -- * All vendors
      juliusCeasar
    , gaiusJuliusCeasar
    , falseAge
    , withCols
    , defaultVal

      -- * PostgreSQL
    , defaultValPostgreSQL
    , returningPostgreSQL
    )
    where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import           Database.Hedsql.Ext
import           Database.Hedsql.SqLite
import qualified Database.Hedsql.PostgreSQL                      as P
import           Database.Hedsql.Drivers.PostgreSQL.Constructor

import           Prelude                                         hiding (null)

--------------------------------------------------------------------------------
-- PRIVATE
--------------------------------------------------------------------------------

-- Define the columns
idC :: Column Int a
idC = col "id" integer

title :: Column String a
title = col "title" $ char 2

firstName :: Column String a
firstName = col "firstName" $ varchar 256

lastName :: Column String a
lastName = col "lastName" $ varchar 256

age :: Column Int a
age = col "age" integer

married :: Column Bool a
married    = col "married" boolean

passportNo :: Column String a
passportNo = col "passportNo" $ varchar 256

father :: Column Int a
father = col "father" integer

countryId :: Column Int a
countryId = col "countryId" integer

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

----------------------------------------
-- All vendors
----------------------------------------

{-|
MariaDB and PosgreSQL:
> INSERT INTO "People" ("id", "title", "firstName", "lastName", "age",
> "married", "passportNo", "countryId", "father")
> VALUES (1, 'Mr', 'Julius', 'Ceasar', 2000, TRUE, NULL, 1, 2)

SqLite:
> INSERT INTO "People" ("id", "title", "firstName", "lastName", "age",
> "married", "passportNo", "father", "countryId")
> VALUES (1, 'Mr', 'Julius', 'Ceasar', 2000, 1, NULL, 2, 2)
-}
juliusCeasar :: InsertStmt a
juliusCeasar =
    insert "People"
        [ assign idC        $ intVal 1
        , assign title      $ stringVal "Mr"
        , assign firstName  $ stringVal "Julius"
        , assign lastName   $ stringVal "Ceasar"
        , assign age        $ intVal 2000
        , assign married    $ boolVal True
        , assign passportNo   null
        , assign father     $ intVal 2
        , assign countryId  $ intVal 2
        ]

{-|
Use the generic 'value' constructor instead of the specialised ones.

MariaDB and PosgreSQL:
> INSERT INTO "People"
> VALUES (1, 'Mr', 'Gaius Julius', 'Ceasar', 2000, TRUE, NULL, NULL, 2)

SqLite:
> INSERT INTO "People"
> ("id", "title", "firstName", "lastName", "age", "married", "passportNo"
> , "father", "countryId")
> VALUES (1, 'Mr', 'Gaius Julius', 'Ceasar', 2000, 1, NULL, NULL, 2)
-}
gaiusJuliusCeasar :: InsertStmt a
gaiusJuliusCeasar =
    insert "People"
        [ assign idC        $ value (2::Int)
        , assign title      $ value "Mr"
        , assign firstName  $ value "Gaius Julius"
        , assign lastName   $ value "Ceasar"
        , assign age        $ value (2000::Int)
        , assign married    $ value True
        , assign passportNo   null
        , assign father       null
        , assign countryId  $ value (2::Int)
        ]

{-|
The below statement is going to fail, because the age is below 0.
> INSERT INTO "People"
> ("title", "firstName", "lastName", "age", "married", "passportNo", "father",
> "countryId")
> VALUES (NULL, 'Mr', 'Julius', 'Ceasar', -1, TRUE, NULL, NULL, 2)
-}
falseAge :: InsertStmt a
falseAge =
    insert "People"
        [ assign title        null
        , assign firstName  $ stringVal "Julius"
        , assign lastName   $ stringVal "Ceasar"
        , assign age        $ intVal (-1)
        , assign married    $ boolVal True
        , assign passportNo   null
        , assign father       null
        , assign countryId  $ intVal 2
        ]

{-|
@
INSERT INTO "People"
  ("title", "firstName", "lastName", "age", "married", "passportNo"
  , "countryId")
  VALUES ('Mr', 'Julius', 'Ceasar', 2000, NULL, NULL, 2)
@
-}
withCols :: InsertStmt a
withCols =
    insert
        "People"
        [ assign title       $ value "Mr"
        , assign firstName   $ value "Julius"
        , assign lastName    $ value "Ceasar"
        , assign age         $ intVal 2000
        , assign married       null
        , assign passportNo    null
        , assign countryId   $ intVal 2
        ]

{-|
This example doesn't define the types of the columns.
@
INSERT INTO "People" (
  "firstName",
  "lastName",
  "age",
  "passportNo",
  "countryId")
VALUES (
  'Julius',
  'Ceasar',
  2000,
  NULL,
  2)
@
-}
defaultVal :: InsertStmt a
defaultVal =
    insert
        "People"
        [ assign "firstName"  $ genQVal "Julius"
        , assign "lastName"   $ genQVal "Ceasar"
        , assign "age"        $ genVal (2000::Int)
        , assign "passportNo"   null
        , assign "countryId"  $ genVal (2::Int)
        ]

----------------------------------------
-- PostgreSQL
----------------------------------------

{-|
> INSERT INTO "People"
> ("title", "firstName", "lastName", "age", "passportNo", "father", "countryId")
> VALUES (DEFAULT, 'Mr', 'Julius', 'Ceasar', 2000, TRUE, NULL, NULL, 2)
-}
defaultValPostgreSQL :: InsertStmt P.PostgreSQL
defaultValPostgreSQL =
    insert "People"
        [ assign idC        $ null
        , assign title      $ value default_
        , assign firstName  $ stringVal "Julius"
        , assign lastName   $ stringVal "Ceasar"
        , assign age        $ intVal 2000
        , assign married    $ boolVal True
        , assign passportNo   null
        , assign father       null
        , assign countryId  $ intVal 2
        ]

{-|
@
INSERT INTO "People" (
  "title",
  "firstName",
  "lastName",
  "age",
  "married",
  "passportNo",
  "countryId",
  "father")
VALUES (
  'Mr',
  'Julius',
  'Ceasar',
  2000,
  TRUE,
  NULL,
  1,
  2)
@
-}
returningPostgreSQL :: InsertStmt P.PostgreSQL
returningPostgreSQL = do
    insert "People"
        [ assign title      $ stringVal "Mr"
        , assign firstName  $ stringVal "Julius"
        , assign lastName   $ stringVal "Ceasar"
        , assign age        $ intVal 2000
        , assign married    $ boolVal True
        , assign passportNo   null
        , assign father     $ intVal 2
        , assign countryId  $ intVal 2
        ]
    P.returning idC

{-|
@
INSERT INTO "People"
  ("title", "firstName", "lastName", "age", "passportNo", "countryId")
  VALUES
  ('Mr', 'Julius', 'Ceasar', 2000, NULL, NULL, 2),
  ('Mr', 'Gnaeus', 'Pompeius', 2000, NULL, NULL, 2)
@
-}
--multiValsPostgreSQL :: Insert P.PostgreSQL
--multiValsPostgreSQL =
--    insert
--        "People"
--        ["title", "firstName", "lastName", "age", "passportNo", "countryId"]
--        [
--        [ wrap $ value "Mr"
--        , wrap $ value "Julius"
--        , wrap $ value "Ceasar"
--        , wrap $ value (2000::Int)
--        , wrap $ value null
--        , wrap $ value null
--        , wrap $ value (2::Int)
--        ]
--        ,
--        [ wrap $ value "Mr"
--        , wrap $ value "Gnaeus"
--        , wrap $ value "Pompeius"
--        , wrap $ value (2000::Int)
--        , wrap $ value null
--        , wrap $ value null
--        , wrap $ value (2::Int)
--        ]
--        ]
