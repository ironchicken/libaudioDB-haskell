-- AudioDB - Haskell bindings to the libaudioDB audio search engine library
--
-- Copyright (C) 2014, 2015 Richard Lewis, Goldsmiths' College
-- Author: richard.lewis@gold.ac.uk
--
-- This module provides a mid-level interface above the basic FFI
-- bindings.

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

{-# LANGUAGE DeriveDataTypeable #-}

module AudioDB where

import           ADB
import           Data.Maybe (isJust, catMaybes)
import           Control.Monad (when)
import           Control.Exception (throw, Exception)
import           Data.Typeable (Typeable)
import           Foreign (Ptr, peek, poke)
import           Foreign.C.Types
import           Foreign.Marshal.Utils (new)
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.C.String (newCString)
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

type Frame = Int
type Seconds = Double
type FrameSize = (Frame -> Seconds)
type FeatureRate = (Seconds -> Frame)

inSeconds :: FrameSize
inSeconds = fromIntegral

inFrames :: FeatureRate
inFrames = ceiling

withSeconds :: FeatureRate -> FrameSize -> (Seconds -> Seconds) -> Frame -> Frame
withSeconds secToFrames framesToSec f frames = (secToFrames . f . framesToSec) frames

withFrames :: FeatureRate -> FrameSize -> (Frame -> Frame) -> Seconds -> Seconds
withFrames secToFrames framesToSec f seconds = (framesToSec . f . secToFrames) seconds

withAudioDB :: (ADBQuerySpec -> ADBResult) -> FeatureRate -> (Ptr ADB) -> ADBResult
withAudioDB = undefined

withExistingAudioDB :: (ADBQuerySpec -> ADBResult) -> FeatureRate -> FilePath -> ADBResult
withExistingAudioDB = undefined

withExistingROAudioDB :: (ADBQuerySpec -> ADBResult) -> FeatureRate -> FilePath -> ADBResult
withExistingROAudioDB = undefined

withNewAudioDB :: (ADBQuerySpec -> ADBResult) -> FeatureRate -> FilePath -> ADBResult
withNewAudioDB = undefined

withADBStatus :: (ADBStatus -> IO a) -> (Ptr ADB) -> IO a
withADBStatus f adb = do
  alloca $ \statusPtr -> do
    res     <- audiodb_status adb statusPtr
    when (res /= 0) (throw DBStatusException)
    status  <- peek statusPtr
    (f status)

type DatumProperties     = (Int, Int, DV.Vector Double, Maybe (DV.Vector Double))
type FeaturesParser      = (FilePath -> IO DatumProperties)
type PowerFeaturesParser = (FilePath -> IO (Maybe (DV.Vector Double)))

_readFeaturesFile :: String                         -- key
                     -> FilePath                    -- features file
                     -> FeaturesParser              -- features parser
                     -> Maybe (FilePath,
                               PowerFeaturesParser) -- power features file, parser
                     -> IO (Maybe ADBDatumPtr)
_readFeaturesFile key featuresFile featuresParser (Just (powersFile, powersParser)) = do
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

_readFeaturesFile key featuresFile featuresParser Nothing = do
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

readCSVFeaturesTimesPowers key featuresFile powersFile = _readFeaturesFile key featuresFile parseCSVFeaturesWithTimesFile    (Just (powersFile, parseCSVPowerFeaturesFile))
readCSVFeaturesPowers key featuresFile powersFile      = _readFeaturesFile key featuresFile parseCSVFeaturesWithoutTimesFile (Just (powersFile, parseCSVPowerFeaturesFile))
readCSVFeaturesTimes key featuresFile                  = _readFeaturesFile key featuresFile parseCSVFeaturesWithTimesFile    Nothing
readCSVFeaturesOnly key featuresFile                   = _readFeaturesFile key featuresFile parseCSVFeaturesWithoutTimesFile Nothing

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

checkDimensions :: (Ptr ADB) -> ADBDatum -> IO Bool
checkDimensions adb datum =
  withADBStatus (\s -> if not (checkDim s datum)
                       then throw $ QueryDimensionsMismatchException (status_dim s) (datum_dim datum)
                       else return True) adb
  where checkDim s d = (status_dim s) == (datum_dim d)

(|||) :: Maybe a -> b -> Maybe b
Just _  ||| b = Just b
Nothing ||| _ = Nothing

(//) :: Maybe a -> a -> a
Just x  // _ = x
Nothing // y = y

-- NOTE The audioDB::query function can be pilfered for tips on
-- implementing query

type RefinementParams = (Maybe ADBKeyList, Maybe ADBKeyList, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Int, Maybe Int)

combineJustRfnParams :: RefinementParams -> RefinementFlag
combineJustRfnParams (incl, excl, rad, absThrsh, relThrsh, durRat, qHopSz, iHopSz) =
  combineRefinementFlags $ catMaybes [(incl     ||| includeKeyListFlag),
                                      (excl     ||| excludeKeyListFlag),
                                      (rad      ||| radiusFlag),
                                      (absThrsh ||| absoluteThresholdFlag),
                                      (relThrsh ||| relativeThresholdFlag),
                                      (durRat   ||| durationRatioFlag),
                                      (qHopSz   ||| hopSizeFlag),
                                      (iHopSz   ||| hopSizeFlag)]

emptyADBKeyList :: ADBKeyList
emptyADBKeyList = ADBKeyList { keylist_nkeys = 0, keylist_keys = [] }

data QueryException = QuerySequenceBoundsException Int Int Int
                    | QueryDimensionsMismatchException Int Int
                    deriving (Show, Typeable)
instance Exception QueryException

data DatabaseException = DBStatusException
                       deriving (Show, Typeable)
instance Exception DatabaseException

-- FIXME Maybe we could have some `type`s here to distinguish all
-- these parameters from each other.
mkQuery :: ADBDatumPtr   -- query datum
           -> Maybe FeatureRate
           -> Maybe Seconds    -- sequence start
           -> Maybe Seconds    -- sequence length
           -> Maybe QueryIDFlag
           -> Maybe AccumulationFlag
           -> Maybe DistanceFlag
           -> Maybe Int        -- number of point nearest neighbours
           -> Maybe Int        -- number of tracks
           -> Maybe ADBKeyList -- include
           -> Maybe ADBKeyList -- exclude
           -> Maybe Double     -- radius
           -> Maybe Double     -- absoluate threshold
           -> Maybe Double     -- relative threshold
           -> Maybe Double     -- duration ratio
           -> Maybe Int        -- query hop size
           -> Maybe Int        -- instance hop size
           -> ADBQuerySpecPtr
           -> IO ()

mkQuery datum secToFrames sqStart sqLen qidFlgs acc dist ptsNN resultLen incl excl rad absThrsh relThrsh durRat qHopSz iHopSz qPtr = do
  let fr = (secToFrames // inFrames)
      qid = ADBQueryID {
        queryid_datum           = datum,
        queryid_sequence_length = fr (sqLen // 16),
        queryid_flags           = (qidFlgs // allowFalsePositivesFlag),
        queryid_sequence_start  = fr (sqStart // 0) }

      params = ADBQueryParameters {
        query_parameters_accumulation = (acc // databaseFlag),
        query_parameters_distance     = (dist // dotProductFlag),
        query_parameters_npoints      = (ptsNN // 10),
        query_parameters_ntracks      = (resultLen // 10) }

      rfnFlgs = combineJustRfnParams (incl, excl, rad, absThrsh, relThrsh, durRat, qHopSz, iHopSz)
      refine = ADBQueryRefine {
        query_refine_flags              = rfnFlgs,
        query_refine_include            = (incl // emptyADBKeyList),
        query_refine_exclude            = (excl // emptyADBKeyList),
        query_refine_radius             = (rad // 1.0),
        query_refine_absolute_threshold = (absThrsh // 0),
        query_refine_relative_threshold = (relThrsh // 0),
        query_refine_duration_ratio     = (durRat // 0),
        query_refine_qhopsize           = (qHopSz // 1),
        query_refine_ihopsize           = (iHopSz // 1) }
      query = ADBQuerySpec {
        query_spec_qid    = qid,
        query_spec_params = params,
        query_spec_refine = refine }

  d <- peek datum

  let q = if (queryid_sequence_start qid) + (queryid_sequence_length qid) < (datum_nvectors d)
          then poke qPtr query
          else throw $ QuerySequenceBoundsException (queryid_sequence_start qid) (queryid_sequence_length qid) (datum_nvectors d)
  q

-- FIXME It's not obvious how to get a QueryAllocator because there
-- are no functions that have it as their return type. In fact, it's
-- just a curried function whose last argument is a
-- ADBQuerySpecPtr. Could we add some sort of mkUnallocXXXQuery
-- function?
type QueryAllocator = (ADBQuerySpecPtr -> IO ())

withQueryPtr :: (Ptr ADB) -> QueryAllocator -> (ADBQuerySpecPtr -> IO a) -> IO a
withQueryPtr adb allocQuery f =
  alloca (\qPtr -> do
             allocQuery qPtr
             datum <- peek qPtr >>= return . queryid_datum . query_spec_qid >>= peek
             dimOk <- checkDimensions adb datum
             (f qPtr))

applyQueryPtr :: (Ptr ADB) -> (ADBQuerySpecPtr -> IO a) -> QueryAllocator -> IO a
applyQueryPtr adb f allocQuery = withQueryPtr adb allocQuery f

withQuery :: (Ptr ADB) -> QueryAllocator -> (ADBQuerySpec -> IO a) -> IO a
withQuery adb allocQuery f =
  alloca (\qPtr -> do
             allocQuery qPtr
             q <- peek qPtr
             datum <- (return . queryid_datum . query_spec_qid) q >>= peek
             dimOk <- checkDimensions adb datum
             (f q))

applyQuery :: (Ptr ADB) -> (ADBQuerySpec -> IO a) -> QueryAllocator -> IO a
applyQuery adb f allocQuery = withQuery adb allocQuery f

-- A 'detached query' is a query that's not associated with an
-- ADB. It's used for query manipulation.
withDetachedQueryPtr :: QueryAllocator -> (ADBQuerySpecPtr -> IO a) -> IO a
withDetachedQueryPtr allocQuery f =
  alloca (\qPtr -> do
             allocQuery qPtr
             (f qPtr))

applyDetachedQueryPtr :: (ADBQuerySpecPtr -> IO a) -> QueryAllocator -> IO a
applyDetachedQueryPtr f allocQuery = withDetachedQueryPtr allocQuery f

withDetachedQuery :: QueryAllocator -> (ADBQuerySpec -> IO a) -> IO a
withDetachedQuery allocQuery f =
  alloca (\qPtr -> do
             allocQuery qPtr
             q <- peek qPtr
             (f q))

applyDetachedQuery :: (ADBQuerySpec -> IO a) -> QueryAllocator -> IO a
applyDetachedQuery f allocQuery = withDetachedQuery allocQuery f

execQuery :: (Ptr ADB) -> QueryAllocator -> IO ADBQueryResults
execQuery adb allocQuery =
  withQueryPtr adb allocQuery (\qPtr -> do { r <- audiodb_query_spec adb qPtr; peek r >>= return })

withResults :: ADBQueryResultsPtr -> (ADBQueryResults -> IO a) -> IO a
withResults rPtr f = do
  r <- peek rPtr
  (f r)

applyResults :: (ADBQueryResults -> IO a) -> ADBQueryResultsPtr -> IO a
applyResults f rPtr = withResults rPtr f

type QueryTransformer = (ADBQueryResultsPtr -> QueryAllocator -> QueryAllocator)
type QueryCallback a = (ADBQueryResultsPtr -> IO a)
type QueryComplete = (QueryAllocator -> ADBQueryResultsPtr -> IO Bool)

queryStart :: (Ptr ADB) -> ADBQuerySpecPtr -> IO ADBQueryResultsPtr
queryStart adb qPtr = audiodb_query_spec adb qPtr

queryStep :: (Ptr ADB) -> ADBQuerySpecPtr -> ADBQueryResultsPtr -> IO ADBQueryResultsPtr
queryStep adb qPtr res = audiodb_query_spec_given_sofar adb qPtr res

thenElseIfM :: (Monad m) => m a -> m a -> Bool -> m a
thenElseIfM t f p = if p then t else f

queryWithCallback :: (Ptr ADB) -> QueryAllocator -> QueryCallback a -> QueryComplete -> IO ADBQueryResultsPtr
queryWithCallback adb alloc callback isFinished =
  withQueryPtr adb alloc (\qPtr -> do
                             let initQ   = queryStart adb qPtr
                                 stepQ r = callback r >> queryStep adb qPtr r >>= iterQ
                                 iterQ r = isFinished alloc r >>= thenElseIfM (return r) (stepQ r)
                             r0 <- initQ
                             iterQ r0)

queryWithTransform :: (Ptr ADB) -> QueryAllocator -> QueryTransformer -> QueryComplete -> IO ADBQueryResultsPtr
queryWithTransform adb alloc transform complete = do
  let initQ     = withQueryPtr adb alloc (\qPtr -> queryStart adb qPtr)
      stepQ a r = withQueryPtr adb a (\qPtr -> queryStep adb qPtr r) >>= iterQ a
      iterQ a r = complete a r >>= thenElseIfM (return r) (stepQ (transform r a) r)
  r0 <- initQ
  iterQ alloc r0

queryWithCallbacksAndTransform :: (Ptr ADB) -> QueryAllocator -> QueryTransformer -> QueryCallback a -> QueryComplete -> IO ADBQueryResultsPtr
queryWithCallbacksAndTransform adb alloc transform callback complete = do
  let initQ     = withQueryPtr adb alloc (\qPtr -> queryStart adb qPtr)
      stepQ a r = callback r >> withQueryPtr adb a (\qPtr -> queryStep adb qPtr r) >>= iterQ a
      iterQ a r = complete a r >>= thenElseIfM (return r) (stepQ (transform r a) r)
  r0 <- initQ
  iterQ alloc r0

mkPointQuery :: ADBDatumPtr   -- query features
                -> Int        -- number of point nearest neighbours
                -> ADBQuerySpecPtr
                -> IO ()
mkPointQuery = undefined

mkTrackQuery :: ADBDatumPtr    -- query features
                -> Int         -- number of point nearest neighbours
                -> Int         -- number of tracks
                -> ADBQuerySpecPtr
                -> IO ()
mkTrackQuery = undefined

mkSequenceQuery :: ADBDatumPtr    -- query features
                   -> FeatureRate
                   -> Int         -- number of point nearest neighbours
                   -> Int         -- number of tracks
                   -> Seconds     -- sequence start
                   -> Seconds     -- sequence length
                   -> Maybe DistanceFlag
                   -> Maybe Double -- absolute power threshold
                   -> ADBQuerySpecPtr
                   -> IO ()
mkSequenceQuery datum secToFrames ptsNN resultLen sqStart sqLen dist absThrsh qPtr =
  mkQuery datum (Just secToFrames) (Just sqStart) (Just sqLen) Nothing (Just perTrackFlag) (dist ||| euclideanNormedFlag) (Just ptsNN) (Just resultLen) Nothing Nothing Nothing (absThrsh ||| 0) Nothing Nothing Nothing Nothing qPtr

execSequenceQuery :: (Ptr ADB)
                     -> ADBDatumPtr -- query features
                     -> FeatureRate
                     -> Int         -- number of point nearest neighbours
                     -> Int         -- number of tracks
                     -> Seconds     -- sequence start
                     -> Seconds     -- sequence length
                     -> Maybe DistanceFlag
                     -> Maybe Double -- absolute power threshold
                     -> IO ADBQueryResults
execSequenceQuery adb datum secToFrames ptsNN resultLen sqStart sqLen dist absThrsh =
  execQuery adb (mkSequenceQuery datum secToFrames ptsNN resultLen sqStart sqLen dist absThrsh)

transformSequenceQuery :: (ADBDatumPtr -> ADBDatumPtr)        -- query features
                          -> FeatureRate
                          -> FrameSize
                          -> (Int -> Int)                     -- number of point nearest neighbours
                          -> (Int -> Int)                     -- number of tracks
                          -> (Seconds -> Seconds)             -- sequence start
                          -> (Seconds -> Seconds)             -- sequence length
                          -> (Maybe DistanceFlag -> Maybe DistanceFlag)
                          -> (Maybe Double -> Maybe Double)   -- absolute power threshold)
                          -> ADBQueryResultsPtr
                          -> QueryAllocator
                          -> ADBQuerySpecPtr
                          -> IO ()
transformSequenceQuery tDatum secToFrames framesToSec tPtsNN tResultLen tSqStart tSqLen tDist tAbsThrsh resPtr fromAlloc toPtr =
  withDetachedQueryPtr fromAlloc $ \fromPtr -> do
    q <- peek fromPtr
    let datum     = tDatum     $ (queryid_datum . query_spec_qid) q
        ptsNN     = tPtsNN     $ (query_parameters_npoints . query_spec_params) q
        resultLen = tResultLen $ (query_parameters_ntracks . query_spec_params) q
        sqStart   = (withSeconds secToFrames framesToSec tSqStart ((queryid_sequence_start . query_spec_qid) q))
        sqLen     = (withSeconds secToFrames framesToSec tSqLen ((queryid_sequence_length . query_spec_qid) q))
        dist      = tDist      $ Just $ (query_parameters_distance . query_spec_params) q
        absThrsh  = tAbsThrsh  $ Just $ (query_refine_absolute_threshold . query_spec_refine) q
    mkSequenceQuery datum secToFrames ptsNN resultLen (framesToSec sqStart) (framesToSec sqLen) dist absThrsh toPtr

mkNSequenceQuery :: ADBDatumPtr  -- query features
                    -> FeatureRate
                    -> Int         -- number of point nearest neighbours
                    -> Int         -- number of tracks
                    -> Seconds     -- sequence start
                    -> Seconds     -- sequence length
                    -> Maybe DistanceFlag
                    -> Maybe Double -- absolute power threshold
                    -> ADBQuerySpecPtr
                    -> IO ()
mkNSequenceQuery datum secToFrames ptsNN resultLen sqStart sqLen dist absThrsh qPtr =
  -- FIXME How do we actually implement nsequence query? In audioDB
  -- the only obvious difference is the type of reporter, but I don't
  -- think that means anything in the library. I've wondered whether
  -- the one_to_one accumulator is the thing. It needs a radius rather
  -- than a pointsNN argument, but from audioDB it just returns lots
  -- of 0 hits.
  mkQuery datum (Just secToFrames) (Just sqStart) (Just sqLen) Nothing (Just oneToOneFlag) (dist ||| euclideanNormedFlag) (Just ptsNN) (Just resultLen) Nothing Nothing Nothing (absThrsh ||| 0) Nothing Nothing Nothing Nothing qPtr

execNSequenceQuery :: (Ptr ADB)
                      -> ADBDatumPtr -- query features
                      -> FeatureRate
                      -> Int         -- number of point nearest neighbours
                      -> Int         -- number of tracks
                      -> Seconds     -- sequence start
                      -> Seconds     -- sequence length
                      -> Maybe DistanceFlag
                      -> Maybe Double -- absolute power threshold
                      -> IO ADBQueryResults
execNSequenceQuery adb datum secToFrames ptsNN resultLen sqStart sqLen dist absThrsh =
  execQuery adb (mkNSequenceQuery datum secToFrames ptsNN resultLen sqStart sqLen dist absThrsh)

mkOneToOneSequenceQuery :: ADBDatumPtr  -- query features
                           -> ADBQuerySpecPtr
                           -> IO ()
mkOneToOneSequenceQuery = undefined

mkSequenceQueryDeltaNTracks :: FeatureRate
                               -> FrameSize
                               -> (Int -> Int)
                               -> ADBQueryResultsPtr
                               -> QueryAllocator
                               -> ADBQuerySpecPtr
                               -> IO ()
mkSequenceQueryDeltaNTracks secToFrames frameToSecs delta = transformSequenceQuery id secToFrames frameToSecs id delta id id id id
