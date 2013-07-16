-- |
-- Module:      Database.Migrate
-- Copyright:   (c) 2012 Mark Hibberd
-- License:     BSD3
-- Maintainer:  Mark Hibberd <mark@hibberd.id.au>
-- Portability: portable
--
-- A library to assist with managing database versioning
-- and migration.
--
--  /Note/: This library is under heavy development, currently
--  the PostgreSQL implementation is functional, but
--  expected to change. It is intended that a type safe
--  migration api, command line tools and MySql support be added
--  before this library will be considered stable.
--
module Database.Migrate (module X) where

import Database.Migrate.Migration as X
import Database.Migrate.PostgreSQL as X
