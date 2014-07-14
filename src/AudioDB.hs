-- AudioDB - Haskell bindings to the libaudioDB audio search engine library
--
-- Copyright (C) 2014 Richard Lewis, Goldsmiths' College
-- Author: richard.lewis@gold.ac.uk
--
-- This module provides a mid-level interface above the basic FFI
-- bindings.

module AudioDB where

import           ADB
import           Data.Maybe (isJust)
import           Foreign (Ptr, peek)
import           Foreign.C.Types
import           Foreign.Marshal.Utils (new)
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.C.String (newCString)
import           System.IO (withFile, IOMode( ReadMode ))
import           System.IO.Unsafe (unsafePerformIO)
import           System.FilePath (FilePath, takeBaseName)
import           Data.CSV (csvFile)
import           Text.Parsec.String (parseFromFile)
import           Text.Parsec.Error (ParseError)
import qualified Data.Vector.Storable as DV

openDB :: FilePath -> (Ptr ADB)
openDB = undefined

closeDB :: (Ptr ADB) -> IO ()
closeDB = undefined

createDB :: FilePath -> (Ptr ADB)
createDB = undefined

withAudioDB :: (ADBQuerySpec -> ADBResult) -> (Ptr ADB) -> ADBResult
withAudioDB = undefined

withExistingAudioDB :: (ADBQuerySpec -> ADBResult) -> FilePath -> ADBResult
withExistingAudioDB = undefined

withNewAudioDB :: (ADBQuerySpec -> ADBResult) -> FilePath -> ADBResult
withNewAudioDB = undefined

withADBStatus :: (ADBStatus -> IO a) -> (Ptr ADB) -> IO a
withADBStatus f adb = do
  alloca $ \statusPtr -> do
    res     <- audiodb_status adb statusPtr
    status  <- peek statusPtr
    (f status)

type DatumProperties     = (Int, Int, DV.Vector Double, Maybe (DV.Vector Double))
type FeaturesParser      = (FilePath -> IO DatumProperties)
type PowerFeaturesParser = (FilePath -> IO (Maybe (DV.Vector Double)))

_readCSVFeatures :: String                         -- key
                    -> FilePath                    -- features file
                    -> FeaturesParser              -- features parser
                    -> Maybe (FilePath,
                              PowerFeaturesParser) -- power features file, parser
                    -> IO (Maybe ADBDatumPtr)
_readCSVFeatures key featuresFile featuresParser (Just (powersFile, powersParser)) = do
  (n, dim, features, times) <- featuresParser featuresFile
  pFeatures                 <- powersParser powersFile
  let power = pFeatures
  datum <- new ADBDatum { datum_nvectors = n,
                          datum_dim      = dim,
                          datum_key      = key,
                          datum_data     = features,
                          datum_power    = power,
                          datum_times    = times }
  return (Just datum)

_readCSVFeatures key featuresFile featuresParser Nothing = do
  (n, dim, features, times) <- featuresParser featuresFile
  datum <- new ADBDatum { datum_nvectors = n,
                          datum_dim      = dim,
                          datum_key      = key,
                          datum_data     = features,
                          datum_power    = Nothing,
                          datum_times    = times }
  return (Just datum)

readCSVFeaturesTimesPowers :: String -> FilePath -> FilePath -> IO (Maybe ADBDatumPtr)
readCSVFeaturesPowers      :: String -> FilePath -> FilePath -> IO (Maybe ADBDatumPtr)
readCSVFeaturesTimes       :: String -> FilePath -> IO (Maybe ADBDatumPtr)
readCSVFeaturesOnly        :: String -> FilePath -> IO (Maybe ADBDatumPtr)

readCSVFeaturesTimesPowers key featuresFile powersFile = _readCSVFeatures key featuresFile parseCSVFeaturesWithTimesFile    (Just (powersFile, parseCSVPowerFeaturesFile))
readCSVFeaturesPowers key featuresFile powersFile      = _readCSVFeatures key featuresFile parseCSVFeaturesWithoutTimesFile (Just (powersFile, parseCSVPowerFeaturesFile))
readCSVFeaturesTimes key featuresFile                  = _readCSVFeatures key featuresFile parseCSVFeaturesWithTimesFile    Nothing
readCSVFeaturesOnly key featuresFile                   = _readCSVFeatures key featuresFile parseCSVFeaturesWithoutTimesFile Nothing

dblHead :: [String] -> Maybe (Double, [String])
dblHead (d:ds) = (Just (read d, ds))
dblHead [] = Nothing

ensureTable :: (Either ParseError [[a]]) -> [[a]]
ensureTable (Left _) = [[]]
ensureTable (Right fs) = fs

takeColumns :: Int -> [[a]] -> [[a]]
takeColumns n (x:xs) = take n x : takeColumns n xs
takeColumns _ [] = []

dropColumns :: Int -> [[a]] -> [[a]]
dropColumns n (x:xs) = drop n x : dropColumns n xs
dropColumns _ [] = []

flattenTakeCols :: Int -> [[a]] -> [a]
flattenTakeCols n = concat . (takeColumns n)

flattenDropCols :: Int -> [[a]] -> [a]
flattenDropCols n = concat . (dropColumns n)

parseCSVFeaturesWithTimesFile :: FeaturesParser
parseCSVFeaturesWithTimesFile fp = do
  o <- parseFromFile csvFile fp
  let features = ensureTable o
      fVec     = DV.unfoldr dblHead (flattenDropCols 1 features)
      tVec     = DV.unfoldr dblHead (flattenTakeCols 1 features)
      nVectors = (length (head features)) - 1

  return (DV.length fVec `div` nVectors, nVectors, fVec, Just tVec)

parseCSVFeaturesWithoutTimesFile :: FeaturesParser
parseCSVFeaturesWithoutTimesFile fp = do
  o <- parseFromFile csvFile fp
  let features = ensureTable o
      fVec     = DV.unfoldr dblHead (concat features)
      nVectors = (length (head features))

  return (DV.length fVec `div` nVectors, nVectors, fVec, Nothing)

parseCSVPowerFeaturesFile :: PowerFeaturesParser
parseCSVPowerFeaturesFile fp = do
  o <- parseFromFile csvFile fp
  let features = ensureTable o
      fVec     = DV.unfoldr dblHead (concat features)

  return (Just fVec)

readChr12Features :: String -> FilePath -> IO (Maybe ADBDatumPtr)
readChr12Features = undefined

parseChr12FeaturesFile :: FeaturesParser
parseChr12FeaturesFile = undefined

readN3Features :: String -> FilePath -> IO (Maybe ADBDatumPtr)
readN3Features = undefined

parseN3FeaturesFile :: FeaturesParser
parseN3FeaturesFile = undefined

featuresFromKey :: (Ptr ADB) -> String -> IO (Maybe ADBDatum)
featuresFromKey adb key = alloca $ \datumPtr -> do
  key'  <- newCString key
  res   <- audiodb_retrieve_datum adb key' datumPtr
  if res /= 0
    then return $ Nothing
    else do datum <- peek datumPtr; return $ Just datum

insertFeatures :: (Ptr ADB) -> ADBDatumPtr -> IO Bool
insertFeatures adb datumPtr =
  withADBStatus (\status -> do
                    let powered = (status_flags status) == powerFlag
                    datum       <- peek datumPtr
                    res         <- if powered == isJust (datum_power datum)
                                   then audiodb_insert_datum adb datumPtr
                                   else return (1 :: CInt)
                    return (res == (0 :: CInt)))
  adb

insertMaybeFeatures :: (Ptr ADB) -> (Maybe ADBDatumPtr) -> IO Bool
insertMaybeFeatures adb datumPtr = do
  maybe (return False)
    (\p -> do { insertFeatures adb p })
    datumPtr

mkQuery :: ADBDatumPtr   -- query datum
           -> Int        -- sequence length
           -> Int        -- sequence start
           -> QueryIDFlag
           -> Int        -- accumulation
           -> Int        -- distance
           -> Int        -- number of point nearest neighbours
           -> Int        -- number of tracks
           -> RefinementFlag
           -> ADBKeyList -- include
           -> ADBKeyList -- exclude
           -> Double     -- radius
           -> Double     -- absoluate threshold
           -> Double     -- relative threshold
           -> Double     -- duration ratio
           -> Int        -- query hop size
           -> Int        -- instance hop size
           -> ADBQuerySpec

mkQuery datum sqLen sqStart qidFlgs acc dist ptsNN nTrks rfnFlgs incl excl rad absThrsh relThrsh durRat qHopSz iHopSz =
  let qid = ADBQueryID { queryid_datum           = datum,
                         queryid_sequence_length = sqLen,
                         queryid_flags           = qidFlgs,
                         queryid_sequence_start  = sqStart }

      params = ADBQueryParameters { query_parameters_accumulation = acc,
                                    query_parameters_distance     = dist,
                                    query_parameters_npoints      = ptsNN,
                                    query_parameters_ntracks      = nTrks }

      refine = ADBQueryRefine { query_refine_flags              = rfnFlgs,
                                query_refine_include            = incl,
                                query_refine_exclude            = excl,
                                query_refine_radius             = rad,
                                query_refine_absolute_threshold = absThrsh,
                                query_refine_relative_threshold = relThrsh,
                                query_refine_duration_ratio     = durRat,
                                query_refine_qhopsize           = qHopSz,
                                query_refine_ihopsize           = iHopSz }

  in ADBQuerySpec { query_spec_qid    = qid,
                    query_spec_params = params,
                    query_spec_refine = refine }

mkPointQuery :: (Ptr ADB)
                -> ADBDatumPtr -- query features
                -> Int         -- number of point nearest neighbours
                -> ADBQuerySpec
mkPointQuery = undefined

mkTrackQuery :: (Ptr ADB)
                -> ADBDatumPtr -- query features
                -> Int         -- number of point nearest neighbours
                -> Int         -- number of tracks
                -> ADBQuerySpec
mkTrackQuery = undefined

mkSequenceQuery :: (Ptr ADB)
                   -> ADBDatumPtr -- query features
                   -> Int         -- number of point nearest neighbours
                   -> Int         -- number of tracks
                   -> ADBQuerySpec
mkSequenceQuery = undefined

mkNSequenceQuery :: (Ptr ADB)
                    -> ADBDatumPtr -- query features
                    -> Int         -- number of point nearest neighbours
                    -> Int         -- number of tracks
                    -> ADBQuerySpec
mkNSequenceQuery = undefined

mkOneToOneSequenceQuery :: (Ptr ADB)
                           -> ADBDatumPtr  -- query features
                           -> ADBQuerySpec
mkOneToOneSequenceQuery = undefined

query :: (Ptr ADB) -> ADBQuerySpec -> ADBQueryResults
query = undefined

startQuery :: (Ptr ADB) -> ADBQuerySpec -> Int -> ADBQueryResults
startQuery = undefined

resumeQuery :: (Ptr ADB) -> ADBQuerySpec -> ADBQueryResults -> Int -> ADBQueryResults
resumeQuery = undefined

completeQuery :: (Ptr ADB) -> ADBQuerySpec -> ADBQueryResults -> ADBQueryResults
completeQuery = undefined
