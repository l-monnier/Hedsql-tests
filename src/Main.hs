module Main where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Database.Hedsql.Tests.Delete
import Database.Hedsql.Tests.Insert
import Database.Hedsql.Tests.Queries
import Database.Hedsql.Tests.TableManipulations
import Database.Hedsql.Tests.Update

import Test.Framework (defaultMain)

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

-- | Run the tests.
main :: IO()
main = defaultMain
    [ Database.Hedsql.Tests.Delete.tests
    , Database.Hedsql.Tests.Insert.tests
    , Database.Hedsql.Tests.Queries.tests
    , Database.Hedsql.Tests.TableManipulations.tests
    , Database.Hedsql.Tests.Update.tests
    , Database.Hedsql.Tests.PrettyPrint.tests
    ]