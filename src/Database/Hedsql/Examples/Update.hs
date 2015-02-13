{-|
Module      : Database/Hedsql/Examples/Update.hs
Description : Collection of UPDATE statements.
Copyright   : (c) Leonard Monnier, 2015
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

A collection of UPDATE statements to be used in tests or as examples.
-}
module Database.Hedsql.Examples.Update
    (
      -- * All vendors
      equalTo
    , updateSelect
    
      -- * PostgreSQL
    , defaultVal
    )
    where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import           Database.Hedsql.SqLite
import           Database.Hedsql.Drivers.PostgreSQL.Constructor
import qualified Database.Hedsql.PostgreSQL                      as P

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

----------------------------------------
-- All vendors
----------------------------------------
        
-- | > UPDATE "People" SET "age" = 2050 WHERE "lastName" = 'Ceasar'
equalTo :: Update a
equalTo =
        update "People" [assign "age" (2050::Int)]
    /++ where_ ("lastName" /== value "Ceasar")

{-|    
UPDATE "People" SET "age" = ("age" + 1) WHERE "countryId" IN
  (SELECT "countryId" FROM "countries" WHERE "name" = 'Italy')
-}
updateSelect :: Update a
updateSelect =
        update "People" [assign "age" $ "age" /+ (1::Int)]
    /++ where_ ("countryId" `in_`subSelect)
    where
        subSelect =
                select "countryId"
            /++ from "countries"
            /++ where_ ("name" /== value "Italy")

----------------------------------------
-- PostgreSQL
----------------------------------------
            
-- | > UPDATE "People" SET "title" = DEFAULT WHERE "personId" = 1
defaultVal :: Update P.PostgreSQL
defaultVal =
        update "People" [assign "title" default_]
    /++ where_ ("personId" /== (1::Int))