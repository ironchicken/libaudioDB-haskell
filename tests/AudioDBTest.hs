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
  datumPtr <- readCSVFeaturesTimes key fp
  maybe (putStrLn "Could no parse.")
    (\p -> do
        d <- peek p
        putStrLn $ show "Key: " ++ (show (datum_key d)) ++
          "; nVectors: " ++ (show (datum_nvectors d)) ++
          "; dim: " ++ (show (datum_dim d)) ++
          "; 100 features: " ++ (show (V.take 100 (datum_data d))) ++
          "; 100 times: " ++ (show (maybe (V.fromList [0]) (\t -> (V.take 100 t)) (datum_times d)))
        free p
    ) datumPtr

main :: IO ()
main = do
  -- FIXME If you reverse the sequence of insertCSVFeatures and
  -- readCSVFeatures, the insert seems to be ineffective (i.e. audioDB
  -- -Z on the resulting DB shows no content)
  fp       <- newCString "wagner.adb"
  adb      <- audiodb_create fp (CUInt 0) (CUInt 0) (CUInt 12)
  l2normed <- audiodb_l2norm adb
  -- powered  <- audiodb_power adb

  datumPtr <- readCSVFeaturesTimes "WandererSceneImplicit" "WandererSceneImplicit_vamp_nnls-chroma_nnls-chroma_chroma.csv"
  inserted <- insertMaybeFeatures adb datumPtr
  putStrLn $ "Inserted 'WandererSceneImplicit': " ++ (show inserted)
  maybe (return ()) (\d -> free d) datumPtr

  test_readCSVFeatures "WandererSceneImplicit" "WandererSceneImplicit_vamp_nnls-chroma_nnls-chroma_chroma.csv"

  putStrLn "Done."
