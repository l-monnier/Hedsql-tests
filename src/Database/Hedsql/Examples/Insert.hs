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
> INSERT INTO "People" VALUES (1, 'Mr', 'Julius', 'Ceasar', 2000, NULL, 1, 2)
-}
juliusCeasar :: Insert a
juliusCeasar = 
    insertInto "People"
        [
        [ value (1::Int)
        , value "Mr"
        , value "Julius"
        , value "Ceasar"
        , value (2000::Int)
        , value null
        , value (2::Int)
        , value (2::Int)
        ]
        ]

{-|
> INSERT INTO "People"
> VALUES (1, 'Mr', 'Gaius Julius', 'Ceasar', 2000, NULL, NULL, 2)
-}
gaiusJuliusCeasar :: Insert a
gaiusJuliusCeasar =
    insertInto "People"
        [
        [ value (2::Int)
        , value "Mr"
        , value "Gaius Julius"
        , value "Ceasar"
        , value (2000::Int)
        , value null
        , value null
        , value (2::Int)
        ]
        ]


{-|
The below statement is going to fail, because the age is below 0.
> INSERT INTO "People"
> VALUES (NULL, 'Mr', 'Julius', 'Ceasar', -1, NULL, NULL, 2)
-}
falseAge :: Insert a      
falseAge = 
    insertInto "People"
        [
        [ value null
        , value "Mr"
        , value "Julius"
        , value "Ceasar"
        , value (-1::Int)
        , value null
        , value null
        , value (2::Int)
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
        [ value "Mr"
        , value "Julius"
        , value "Ceasar"
        , value (2000::Int)
        , value null
        , value null
        , value (2::Int)
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
        [ value "Julius"
        , value "Ceasar"
        , value (2000::Int)
        , value null
        , value null
        , value (2::Int)
        ]
        ]

----------------------------------------
-- PostgreSQL
----------------------------------------

{-|
> INSERT INTO "People"
> VALUES (DEFAULT, 'Mr', 'Julius', 'Ceasar', 2000, NULL, NULL, 2)
-}
defaultValPostgreSQL :: Insert P.PostgreSQL
defaultValPostgreSQL = 
    insertInto "People"
        [
        [ value null
        , value default_
        , value "Julius"
        , value "Ceasar"
        , value (2000::Int)
        , value null
        , value null
        , value (2::Int)
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
        [ value "Mr"
        , value "Julius"
        , value "Ceasar"
        , value (2000::Int)
        , value null
        , value null
        , value (2::Int)
        ]
        ,
        [ value "Mr"
        , value "Gnaeus"
        , value "Pompeius"
        , value (2000::Int)
        , value null
        , value null
        , value (2::Int)
        ]
        ]