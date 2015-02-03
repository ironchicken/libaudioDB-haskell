module Main where

import           ADB
import           AudioDB
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

sample_rate :: Int
sample_rate = 44100

step_size :: Int
step_size = 2048

framesPerSecond :: Double
framesPerSecond = sr / ss
  where sr = fromIntegral sample_rate
        ss = fromIntegral step_size

test_query :: FilePath -> FilePath -> FilePath -> Seconds -> Seconds -> IO ()
test_query adbFile queryFile qPowersFile start len = do
  adbFN  <- newCString adbFile
  adb    <- audiodb_open adbFN 0
  if adb == nullPtr then putStrLn $ "Could not open " ++ (show adbFile) else putStrLn $ "Opened " ++ (show adbFile)
  queryFeatures <- readCSVFeaturesTimesPowers "chester_16" queryFile qPowersFile
  maybe (putStrLn $ "Could not parse " ++ queryFile)
    (\p -> do
        putStrLn $ "Parsed " ++ queryFile
        res <- execSequenceQuery adb p (floor . (* framesPerSecond)) 1 25 start len (Just euclideanNormedFlag) Nothing
        putStrLn (show res)
    )
    queryFeatures

main :: IO ()
main = do
  -- FIXME If you reverse the sequence of insertCSVFeatures and
  -- readCSVFeatures, the insert seems to be ineffective (i.e. audioDB
  -- -Z on the resulting DB shows no content)
  -- fp       <- newCString "wagner.adb"
  -- adb      <- audiodb_create fp (CUInt 0) (CUInt 0) (CUInt 12)

  -- datumPtr <- readCSVFeaturesTimes "WandererSceneImplicit" "WandererSceneImplicit_vamp_nnls-chroma_nnls-chroma_chroma.csv"
  -- inserted <- insertMaybeFeatures adb datumPtr
  -- putStrLn $ "Inserted 'WandererSceneImplicit': " ++ (show inserted)
  -- maybe (return ()) (\d -> free d) datumPtr

  -- features <- featuresFromKey adb "WandererSceneImplicit"
  -- maybe (putStrLn "Could not retrieve 'WandererSceneImplicit'") (\f -> do putStrLn $ "Found '" ++ (datum_key f) ++ "'") features

  -- -- test_readCSVFeatures "WandererSceneImplicit" "WandererSceneImplicit_vamp_nnls-chroma_nnls-chroma_chroma.csv"

  test_query "parsifal_chroma.adb" "chester_16_chroma.csv" "chester_16_power.csv" 8 11

  putStrLn "Done."
