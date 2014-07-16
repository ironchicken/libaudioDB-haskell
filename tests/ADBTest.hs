module Main where

import           ADB
import           Foreign
import           Foreign.C.Types
import           Foreign.C.String
import qualified Data.Vector.Storable as DV

test_audiodb_insert_datum :: Ptr ADB -> IO (CInt)
test_audiodb_insert_datum adb = do
  let features = DV.fromList [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      power    = Just (DV.fromList [0.0, 0.0, 0.0, 0.0, 0.0, 0.0])
      times    = Nothing -- Just (DV.fromList [0.0, 0.0, 0.0, 0.0, 0.0, 0.0])
      datum    = ADBDatum { datum_nvectors = 6,
                            datum_dim      = 2,
                            datum_key      = "Test",
                            datum_data     = features,
                            datum_power    = power,
                            datum_times    = times }

  with datum (audiodb_insert_datum adb)

test_audiodb_insert_reference :: Ptr ADB -> IO (CInt)
test_audiodb_insert_reference adb = do
  let features  = "features"
      power     = "power"
      key       = "key"
      times     = "times"
      reference = ADBReference { reference_features = features,
                                 reference_power    = power,
                                 reference_key      = key,
                                 reference_times    = times }

  with reference (audiodb_insert_reference adb)

test_audiodb_retrieve_datum :: Ptr ADB -> String -> IO (CInt)
test_audiodb_retrieve_datum adb key = do
  alloca $ \datumPtr -> do
    key'  <- newCString key
    res   <- audiodb_retrieve_datum adb key' datumPtr
    datum <- peek datumPtr

    -- putStrLn $ "datumPtr @ " ++ (show datumPtr)
    putStrLn $ (show (datum_key datum)) ++ (show (datum_nvectors datum))
    -- putStrLn $ (show datum)

    return res

test_audiodb_status :: Ptr ADB -> IO (CInt)
test_audiodb_status adb = do
  alloca $ \statusPtr -> do
    res    <- audiodb_status adb statusPtr
    status <- peek statusPtr

    putStrLn $ (show status)

    return res

test_audiodb_liszt :: Ptr ADB -> IO ()
test_audiodb_liszt adb = do
  lisztPtr <- audiodb_liszt adb
  liszt    <- peek lisztPtr

  putStrLn $ (show liszt)

test_audiodb_dump :: Ptr ADB -> String -> IO ()
test_audiodb_dump adb path = do
  path' <- newCString path
  dump_res <- audiodb_dump adb path'
  putStrLn $ "audiodb_dump -> " ++ (show dump_res)

main :: IO ()
main = do
  fp       <- newCString "test.adb"
  adb      <- audiodb_create fp (CUInt 0) (CUInt 0) (CUInt 2)
  l2normed <- audiodb_l2norm adb
  powered  <- audiodb_power adb
  putStrLn $ (show fp) ++ ": l2normed: " ++ (show l2normed) ++ "; powered: " ++ (show powered)

  insert_res   <- test_audiodb_insert_datum adb
  putStrLn $ "test_audiodb_insert_datum: " ++ (show insert_res)
  retrieve_res <- test_audiodb_retrieve_datum adb "Test"
  putStrLn $ "test_audiodb_retrieve_datum: " ++ (show retrieve_res)
  inref_res    <- test_audiodb_insert_reference adb
  putStrLn $ "test_audiodb_insert_reference: " ++ (show inref_res)
  status_res   <- test_audiodb_status adb
  putStrLn $ "test_audiodb_status: " ++ (show status_res)

  -- fp <- newCString "chromarichard.n3.db"
  -- adb <- audiodb_open fp (CInt 0)

  -- retrieve_res <- test_audiodb_retrieve_datum adb "0cf7d7a6d24890fabcfe417ba3ebb792"
  -- status_res   <- test_audiodb_status adb
  -- test_audiodb_liszt adb
  -- -- test_audiodb_dump adb "chromarichard.n3.db.dump"

  putStrLn "Done."
