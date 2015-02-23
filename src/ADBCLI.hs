-- AudioDB - Haskell bindings to the libaudioDB audio search engine library
-- 
-- Copyright (C) 2014 Richard Lewis, Goldsmiths' College
-- Author: richard.lewis@gold.ac.uk

-- This file is part of libaudioDB-haskell.

-- libaudioDB-haskell is free software: you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation, either version 3 of
-- the License, or (at your option) any later version.

-- libaudioDB-haskell is distributed in the hope that it will be
-- useful, but WITHOUT ANY WARRANTY; without even the implied warranty
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with libaudioDB-haskell. If not, see <http://www.gnu.org/licenses/>.

module ADBCLI where

import AudioDB

main :: IO ()
main = do
  putStrLn "adb"
