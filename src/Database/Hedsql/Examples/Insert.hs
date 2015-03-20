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
    , multiValsPostgreSQL
    )
    where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import           Database.Hedsql.SqLite
import qualified Database.Hedsql.PostgreSQL                      as P
import           Database.Hedsql.Drivers.PostgreSQL.Constructor

import           Prelude                                         hiding (null)

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

----------------------------------------
-- All vendors
----------------------------------------
        
{-|
MariaDB and PosgreSQL:
> INSERT INTO "People"
> VALUES (1, 'Mr', 'Julius', 'Ceasar', 2000, TRUE, NULL, 1, 2)

SqLite:
> INSERT INTO "People"
> VALUES (1, 'Mr', 'Julius', 'Ceasar', 2000, 1, NULL, 1, 2)
-}
juliusCeasar :: Insert a
juliusCeasar = 
    insertInto "People"
        [
        [ wrap $ value (1::Int)
        , wrap $ value "Mr"
        , wrap $ value "Julius"
        , wrap $ value "Ceasar"
        , wrap $ value (2000::Int)
        , wrap $ value True
        , wrap $ value null
        , wrap $ value (2::Int)
        , wrap $ value (2::Int)
        ]
        ]

{-|
MariaDB and PosgreSQL:
> INSERT INTO "People"
> VALUES (1, 'Mr', 'Gaius Julius', 'Ceasar', 2000, TRUE, NULL, NULL, 2)

SqLite:
> INSERT INTO "People"
> VALUES (1, 'Mr', 'Gaius Julius', 'Ceasar', 2000, 1, NULL, NULL, 2)
-}
gaiusJuliusCeasar :: Insert a
gaiusJuliusCeasar =
    insertInto "People"
        [
        [ wrap $ value (2::Int)
        , wrap $ value "Mr"
        , wrap $ value "Gaius Julius"
        , wrap $ value "Ceasar"
        , wrap $ value (2000::Int)
        , wrap $ value True
        , wrap $ value null
        , wrap $ value null
        , wrap $ value (2::Int)
        ]
        ]

{-|
The below statement is going to fail, because the age is below 0.
> INSERT INTO "People"
> VALUES (NULL, 'Mr', 'Julius', 'Ceasar', -1, TRUE, NULL, NULL, 2)
-}
falseAge :: Insert a      
falseAge = 
    insertInto "People"
        [
        [ wrap $ value null
        , wrap $ value "Mr"
        , wrap $ value "Julius"
        , wrap $ value "Ceasar"
        , wrap $ value (-1::Int)
        , wrap $ value True
        , wrap $ value null
        , wrap $ value null
        , wrap $ value (2::Int)
        ]
        ]

{-|        
INSERT INTO "People"
  ("title", "firstName", "lastName", "age", "passportNo", "countryId")
  VALUES ('Mr', 'Julius', 'Ceasar', 2000, NULL, NULL, 2)
-}
withCols :: Insert a
withCols = 
    insertIntoCols
        "People"
        ["title", "firstName", "lastName", "age", "passportNo", "countryId"]
        [
        [ wrap $ value "Mr"
        , wrap $ value "Julius"
        , wrap $ value "Ceasar"
        , wrap $ value (2000::Int)
        , wrap $ value null
        , wrap $ value null
        , wrap $ value (2::Int)
        ]
        ]

{-|
INSERT INTO "People"
  ("firstName", "lastName", "age", "passportNo", "countryId")
  VALUES ('Julius', 'Ceasar', 2000, NULL, NULL, 2)
-}
defaultVal :: Insert a
defaultVal =
    insertIntoCols
        "People"
        ["firstName", "lastName", "age", "passportNo", "countryId"]
        [
        [ wrap $  value "Julius"
        , wrap $  value "Ceasar"
        , wrap $  value (2000::Int)
        , wrap $  value null
        , wrap $  value null
        , wrap $  value (2::Int)
        ]
        ]

----------------------------------------
-- PostgreSQL
----------------------------------------

{-|
> INSERT INTO "People"
> VALUES (DEFAULT, 'Mr', 'Julius', 'Ceasar', 2000, TRUE, NULL, NULL, 2)
-}
defaultValPostgreSQL :: Insert P.PostgreSQL
defaultValPostgreSQL = 
    insertInto "People"
        [
        [ wrap $ value null
        , wrap $ value default_
        , wrap $ value "Julius"
        , wrap $ value "Ceasar"
        , wrap $ value (2000::Int)
        , wrap $ value True
        , wrap $ value null
        , wrap $ value null
        , wrap $ value (2::Int)
        ]
        ]

{-|        
INSERT INTO "People"
  ("title", "firstName", "lastName", "age", "passportNo", "countryId")
  VALUES
  ('Mr', 'Julius', 'Ceasar', 2000, NULL, NULL, 2),
  ('Mr', 'Gnaeus', 'Pompeius', 2000, NULL, NULL, 2)
-}
multiValsPostgreSQL :: Insert P.PostgreSQL
multiValsPostgreSQL = 
    insertIntoCols
        "People"
        ["title", "firstName", "lastName", "age", "passportNo", "countryId"]
        [
        [ wrap $ value "Mr"
        , wrap $ value "Julius"
        , wrap $ value "Ceasar"
        , wrap $ value (2000::Int)
        , wrap $ value null
        , wrap $ value null
        , wrap $ value (2::Int)
        ]
        ,
        [ wrap $ value "Mr"
        , wrap $ value "Gnaeus"
        , wrap $ value "Pompeius"
        , wrap $ value (2000::Int)
        , wrap $ value null
        , wrap $ value null
        , wrap $ value (2::Int)
        ]
        ]