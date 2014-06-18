module Main where

import           ADB
import           AudioDB
import           Data.Maybe
import           Foreign
import           Foreign.C.Types
import           Foreign.C.String
import qualified Data.Vector.Storable as V

test_readCSVFeatures :: String -> FilePath -> IO ()
test_readCSVFeatures key fp = do
  datum <- readCSVFeaturesTimes key fp
  if isJust datum then
    putStrLn $ show $ fmap (\d -> "Key: " ++ (show (datum_key d)) ++
                                  "; nVectors: " ++ (show (datum_nvectors d)) ++
                                  "; dim: " ++ (show (datum_dim d)) ++
                                  "; 100 features: " ++ (show (V.take 100 (datum_data d))) ++
                                  "; 100 times: " ++ (show (maybe (V.fromList [0]) (\t -> (V.take 100 t)) (datum_times d)))) datum
    else putStrLn "Could not parse."

test_insertCSVFeatures :: Ptr ADB -> String -> FilePath -> IO (CInt)
test_insertCSVFeatures adb key fp = do
  datum <- readCSVFeaturesTimes key fp
  res <- maybe (return (99 :: CInt))
         (\d -> do
             ptr <- new d
             res <- audiodb_insert_datum adb ptr
             return res)
         datum
  return res

main :: IO ()
main = do
  -- test_readCSVFeatures "WandererSceneImplicit" "WandererSceneImplicit_vamp_nnls-chroma_nnls-chroma_chroma.csv"

  fp       <- newCString "wagner.adb"
  adb      <- audiodb_create fp (CUInt 0) (CUInt 0) (CUInt 12)
  l2normed <- audiodb_l2norm adb
  -- powered  <- audiodb_power adb
  insert   <- test_insertCSVFeatures adb "WandererSceneImplicit" "WandererSceneImplicit_vamp_nnls-chroma_nnls-chroma_chroma.csv"

  putStrLn "Done."
