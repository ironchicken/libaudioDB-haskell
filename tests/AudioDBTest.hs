module Main where

import           ADB
import           AudioDB
import           Foreign
import           Foreign.C.Types
import           Foreign.C.String
import           Numeric
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

framesToSeconds :: FrameSize
framesToSeconds f = (fromIntegral f) / framesPerSecond

showResults :: ADBQueryResults -> String
showResults r =
  (show n) ++ " hits:\n" ++ unlines (map showResult results)
  where
    n       = (query_results_nresults r)
    results = (query_results_results r)

showResult :: ADBResult -> String
showResult r =
  q ++ " (@ " ++ (qp ") is in track ") ++ k ++ " @ " ++ (pos "; distance is ") ++ (dist "")
  where
    q    = (result_qkey r)
    qp   = showFFloat nd (framesToSeconds (result_qpos r))
    k    = (result_ikey r)
    pos  = showFFloat nd (framesToSeconds (result_ipos r))
    dist = showFFloat nd ((result_dist r))
    nd   = Just 2

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
        putStrLn (showResults res)
    )
    queryFeatures

test_callback_query :: FilePath -> FilePath -> FilePath -> Seconds -> Seconds -> IO ()
test_callback_query adbFile queryFile qPowersFile start len = do
  adbFN  <- newCString adbFile
  adb    <- audiodb_open adbFN 0
  if adb == nullPtr then putStrLn $ "Could not open " ++ (show adbFile) else putStrLn $ "Opened " ++ (show adbFile)
  queryFeatures <- readCSVFeaturesTimesPowers "chester_16" queryFile qPowersFile
  maybe (putStrLn $ "Could not parse " ++ queryFile)
    (\datumPtr -> do
        let ntracks          = query_parameters_ntracks . query_spec_params
            qAlloc           = mkSequenceQuery datumPtr (floor . (* framesPerSecond)) 1 5 start len (Just euclideanNormedFlag) Nothing
            isFinished _ _ _ = putStrLn "isFinished..." >> return False -- withQueryPtr adb a (\qPtr -> do { q <- peek qPtr; return $ ntracks q >= 25 })
            callback i r     = do
              res <- peek r
              n <- return $ query_results_nresults res
              putStrLn $ "Callback #" ++ (show i) ++ " says: " ++ (show n)
              putStrLn (showResults res)
              return n

        putStrLn $ "Parsed " ++ queryFile
        _ <- queryWithCallback adb qAlloc callback isFinished
        return ())
    queryFeatures

test_transform_query :: FilePath -> FilePath -> FilePath -> Seconds -> Seconds -> IO ()
test_transform_query adbFile queryFile qPowersFile start len = do
  adbFN  <- newCString adbFile
  adb    <- audiodb_open adbFN 0
  if adb == nullPtr then putStrLn $ "Could not open " ++ (show adbFile) else putStrLn $ "Opened " ++ (show adbFile)
  queryFeatures <- readCSVFeaturesTimesPowers "chester_16" queryFile qPowersFile
  maybe (putStrLn $ "Could not parse " ++ queryFile)
    (\datumPtr -> do
        let ntracks          = query_parameters_ntracks . query_spec_params
            qAlloc           = mkSequenceQuery datumPtr (floor . (* framesPerSecond)) 1 5 start len (Just euclideanNormedFlag) Nothing
            isFinished _ _ r = withResults r (\res -> return $ (query_results_nresults res) >= 20)
            transform _ r a  = mkSequenceQueryDeltaNTracks (floor . (* framesPerSecond)) framesToSeconds (\x -> x + 5) r a

        putStrLn $ "Parsed " ++ queryFile
        res <- queryWithTransform adb qAlloc transform isFinished
        putStrLn "Final results:"
        withResults res (\r -> putStrLn (showResults r))
        return ())
    queryFeatures

test_callbacktransform_query :: FilePath -> FilePath -> FilePath -> Seconds -> Seconds -> IO ()
test_callbacktransform_query adbFile queryFile qPowersFile start len = do
  adbFN  <- newCString adbFile
  adb    <- audiodb_open adbFN 0
  if adb == nullPtr then putStrLn $ "Could not open " ++ (show adbFile) else putStrLn $ "Opened " ++ (show adbFile)
  queryFeatures <- readCSVFeaturesTimesPowers "chester_16" queryFile qPowersFile
  maybe (putStrLn $ "Could not parse " ++ queryFile)
    (\datumPtr -> do
        let ntracks          = query_parameters_ntracks . query_spec_params
            qAlloc           = mkSequenceQuery datumPtr (floor . (* framesPerSecond)) 1 5 start len (Just euclideanNormedFlag) Nothing
            isFinished _ a _ = withQuery adb a (\q -> do { return $ (framesToSeconds ((queryid_sequence_length . query_spec_qid) q)) >= 11 }) --withResults r (\res -> return $ (query_results_nresults res) >= 20)
            callback i r     = withResults r (\res -> do { putStrLn $ "#" ++ (show i) ++ ": " ++ (showResults res); return $ (query_results_nresults res) })
            transform _ r a  = mkSequenceQueryDeltaSqLength (floor . (* framesPerSecond)) framesToSeconds (\x -> x + 3) r a

        putStrLn $ "Parsed " ++ queryFile
        res <- queryWithCallbacksAndTransform adb qAlloc transform callback isFinished
        putStrLn "Final results:"
        withResults res (\r -> putStrLn (showResults r))
        return ())
    queryFeatures

test_rotation_query :: FilePath -> FilePath -> FilePath -> Seconds -> Seconds -> [Int] -> IO ()
test_rotation_query adbFile queryFile qPowersFile start len rotations = do
  adbFN  <- newCString adbFile
  adb    <- audiodb_open adbFN 0
  if adb == nullPtr then putStrLn $ "Could not open " ++ (show adbFile) else putStrLn $ "Opened " ++ (show adbFile)
  queryFeatures <- readCSVFeaturesTimesPowers "chester_16" queryFile qPowersFile
  maybe (putStrLn $ "Could not parse " ++ queryFile)
    (\p -> do
        putStrLn $ "Parsed " ++ queryFile
        res <- execSequenceQueryWithRotation adb p (floor . (* framesPerSecond)) framesToSeconds 1 25 start len (Just euclideanNormedFlag) Nothing rotations
        putStrLn (showResults res)
    )
    queryFeatures

db_file :: String
db_file = undefined

test_features_name :: String
test_features_name = undefined

test_features_file :: String
test_features_file = undefined

test_power_features_file :: String
test_power_features_file = undefined

query_seq_start :: Seconds
query_seq_start = undefined

query_seq_length :: Seconds
query_seq_length = undefined

main :: IO ()
main = do
  -- FIXME If you reverse the sequence of insertCSVFeatures and
  -- readCSVFeatures, the insert seems to be ineffective (i.e. audioDB
  -- -Z on the resulting DB shows no content)
  -- fp       <- newCString db_file
  -- adb      <- audiodb_create fp (CUInt 0) (CUInt 0) (CUInt 12)

  -- datumPtr <- readCSVFeaturesTimes test_features_name test_features_file
  -- inserted <- insertMaybeFeatures adb datumPtr
  -- putStrLn $ "Inserted '" ++ test_features_name ++ "': " ++ (show inserted)
  -- maybe (return ()) (\d -> free d) datumPtr

  -- features <- featuresFromKey adb test_features_name
  -- maybe (putStrLn ("Could not retrieve '" ++ test_features_name ++ "'")) (\f -> do putStrLn $ "Found '" ++ (datum_key f) ++ "'") features

  -- test_readCSVFeatures test_features_name test_features_file

  -- test_query db_file test_features_file test_power_features_file query_seq_start query_seq_length
  -- test_transform_query db_file test_features_file test_power_features_file query_seq_start query_seq_length
  -- test_callbacktransform_query db_file test_features_file test_power_features_file query_seq_start query_seq_length
  -- test_rotation_query db_file test_features_file test_power_features_file query_seq_start query_seq_length [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]

  putStrLn "Done."
