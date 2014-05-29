module Main where

import ADB

import Foreign.C.Types
import Foreign.C.String
--import Data.Vector.Storable
--import qualified Data.Vector.Storable as V

main :: IO ()
main = do
  withCString "test.adb" (\path -> do
                             adb <- audiodb_create path (CUInt 0) (CUInt 0) (CUInt 0)
                             l2normed <- audiodb_l2norm adb
                             powered <- audiodb_power adb
                             --unsafeWith (\d -> audiodb_insert_datum d)
                             return ())
