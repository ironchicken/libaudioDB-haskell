-- AudioDB - Haskell bindings to the libaudioDB audio search engine library
--
-- Copyright (C) 2014, 2015 Richard Lewis, Goldsmiths' College
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

module Sound.Audio.Database.Query ( QueryAllocator
                                  , QueryTransformer
                                  , QueryCallback
                                  , QueryComplete
                                  , querySinglePass
                                  , querySinglePassPtr
                                  , withQuery
                                  , applyQuery
                                  , withResults
                                  , applyResults
                                  , queryWithCallback
                                  , queryWithTransform
                                  , queryWithCallbacksAndTransform
                                  , query
                                  , mkPointQuery
                                  , mkTrackQuery
                                  , mkSequenceQuery
                                  , execSequenceQuery
                                  , transformSequenceQuery
                                  , mkNSequenceQuery
                                  , execNSequenceQuery
                                  , mkSequenceQueryDeltaNTracks
                                  , mkSequenceQueryMutateDatum
                                  , mkSequenceQueryWithRotation
                                  , execSequenceQueryWithRotation ) where

import           AudioDB.API
import           Control.Exception (throw)
import qualified Data.Vector.Storable as DV
import           Data.Maybe (catMaybes)
import           Foreign (Ptr, peek, poke)
import           Foreign.Marshal.Alloc (alloca)
import           Sound.Audio.Database
import           Sound.Audio.Database.Types
import Debug.Trace

(|||) :: Maybe a -> b -> Maybe b
Just _  ||| b = Just b
Nothing ||| _ = Nothing

(//) :: Maybe a -> a -> a
Just x  // _ = x
Nothing // y = y

type QueryAllocator   = (ADBQuerySpecPtr -> IO ())
type QueryTransformer = (Int -> ADBQueryResultsPtr -> QueryAllocator -> QueryAllocator)
type QueryCallback a  = (Int -> ADBQueryResultsPtr -> IO a)
type QueryComplete    = (Int -> QueryAllocator -> ADBQueryResultsPtr -> IO Bool)

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
      querySpec = ADBQuerySpec {
        query_spec_qid    = qid,
        query_spec_params = params,
        query_spec_refine = refine }

  d <- peek datum

  putStrLn $ "FIXME: Why does this re-appear? query sequence start: "
    ++ show (queryid_sequence_start qid)
    ++ "; query sequence length: "
    ++ show (queryid_sequence_length qid)
    ++ "; datum length: "
    ++ show (datum_nvectors d)

  let q = if (queryid_sequence_start qid) + (queryid_sequence_length qid) < (datum_nvectors d)
          then poke qPtr querySpec
          else throw $ QuerySequenceBoundsException (queryid_sequence_start qid) (queryid_sequence_length qid) (datum_nvectors d)
  q

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
             let showQID myQPtr = do
                   myQ <- peek myQPtr
                   let d = (queryid_datum . query_spec_qid) myQ
                   putStrLn $ "withDetachedQueryPtr called query allocator @ " ++ (show qPtr) ++ "; datum @ " ++ (show d)
             showQID qPtr
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

querySinglePass :: (Ptr ADB) -> QueryAllocator -> IO ADBQueryResults
querySinglePass adb allocQuery =
  withQueryPtr adb allocQuery (\qPtr -> do { r <- audiodb_query_spec adb qPtr; peek r >>= return })

querySinglePassPtr :: (Ptr ADB) -> QueryAllocator -> IO ADBQueryResultsPtr
querySinglePassPtr adb allocQuery =
  withQueryPtr adb allocQuery (\qPtr -> audiodb_query_spec adb qPtr)

withResults :: ADBQueryResultsPtr -> (ADBQueryResults -> IO a) -> IO a
withResults rPtr f = do
  r <- peek rPtr
  (f r)

applyResults :: (ADBQueryResults -> IO a) -> ADBQueryResultsPtr -> IO a
applyResults f rPtr = withResults rPtr f

queryStart :: (Ptr ADB) -> ADBQuerySpecPtr -> IO ADBQueryResultsPtr
queryStart adb qPtr = audiodb_query_spec adb qPtr

queryStep :: (Ptr ADB) -> ADBQuerySpecPtr -> ADBQueryResultsPtr -> IO ADBQueryResultsPtr
queryStep adb qPtr res = audiodb_query_spec_given_sofar adb qPtr res

thenElseIfM :: (Monad m) => m a -> m a -> Bool -> m a
thenElseIfM t f p = if p then t else f

-- NOTE: This collection of multi-pass query execution functions make
-- the single call to initQ (which is always the queryStart function)
-- with i = 0; then the *first* call to iterQ will be made with i =
-- 1. So you never get a call to iterQ with i = 0. Consequently, when
-- you want to use the step argument to a QueryAllocator,
-- QueryTransformer, or QueryComplete function, you need to bear in
-- mind that you start from 1, at the second iteration.

queryWithCallbackPtr :: (Ptr ADB) -> QueryAllocator -> QueryCallback a -> QueryComplete -> IO ADBQueryResultsPtr
queryWithCallbackPtr adb alloc callback isFinished =
  withQueryPtr adb alloc (\qPtr -> do
                             let iteration = 0
                                 initQ _   = queryStart adb qPtr
                                 stepQ i r = callback i r >> queryStep adb qPtr r >>= iterQ (i + 1)
                                 iterQ i r = isFinished i alloc r >>= thenElseIfM (return r) (stepQ i r)
                             r0 <- initQ iteration
                             iterQ (iteration + 1) r0)

queryWithCallback :: (Ptr ADB) -> QueryAllocator -> QueryCallback a -> QueryComplete -> IO ADBQueryResults
queryWithCallback adb alloc callback isFinished =
  queryWithCallbackPtr adb alloc callback isFinished >>= peek

queryWithTransformPtr :: (Ptr ADB) -> QueryAllocator -> QueryTransformer -> QueryComplete -> IO ADBQueryResultsPtr
queryWithTransformPtr adb alloc transform complete = do
  let iteration   = 0
      initQ _     = trace "Evaluating initQ" withQueryPtr adb alloc (\qPtr -> queryStart adb qPtr)
      stepQ i a r = trace ("Evaluating stepQ with i = " ++ (show i) ++ "; r @ " ++ (show r)) withQueryPtr adb a (\qPtr -> queryStep adb qPtr r) >>= iterQ (i + 1) a
      iterQ i a r = trace ("Evaluating iterQ with i = " ++ (show i) ++ "; r @ " ++ (show r)) complete i a r >>= thenElseIfM (return r) (stepQ i (transform i r a) r)
  r0 <- initQ iteration
  iterQ (iteration + 1) alloc r0

queryWithTransform :: (Ptr ADB) -> QueryAllocator -> QueryTransformer -> QueryComplete -> IO ADBQueryResults
queryWithTransform adb alloc transform complete =
  queryWithTransformPtr adb alloc transform complete >>= peek

queryWithCallbacksAndTransformPtr :: (Ptr ADB) -> QueryAllocator -> QueryTransformer -> QueryCallback a -> QueryComplete -> IO ADBQueryResultsPtr
queryWithCallbacksAndTransformPtr adb alloc transform callback complete = do
  let iteration   = 0
      initQ _     = withQueryPtr adb alloc (\qPtr -> queryStart adb qPtr)
      stepQ i a r = callback i r >> withQueryPtr adb a (\qPtr -> queryStep adb qPtr r) >>= iterQ (i + 1) a
      iterQ i a r = complete i a r >>= thenElseIfM (return r) (stepQ i (transform i r a) r)
  r0 <- initQ iteration
  iterQ (iteration + 1) alloc r0

queryWithCallbacksAndTransform :: (Ptr ADB) -> QueryAllocator -> QueryTransformer -> QueryCallback a -> QueryComplete -> IO ADBQueryResults
queryWithCallbacksAndTransform adb alloc transform callback complete =
  queryWithCallbacksAndTransformPtr adb alloc transform callback complete >>= peek

queryPtr :: (Ptr ADB) -> QueryAllocator -> Maybe QueryTransformer -> Maybe (QueryCallback a) -> Maybe QueryComplete -> IO ADBQueryResultsPtr
queryPtr adb alloc Nothing          Nothing         Nothing           = querySinglePassPtr adb alloc
queryPtr adb alloc (Just transform) Nothing         (Just isFinished) = queryWithTransformPtr adb alloc transform isFinished
queryPtr adb alloc Nothing          (Just callback) (Just isFinished) = queryWithCallbackPtr adb alloc callback isFinished
queryPtr adb alloc (Just transform) (Just callback) (Just isFinished) = queryWithCallbacksAndTransformPtr adb alloc transform callback isFinished
queryPtr adb alloc Nothing          Nothing         (Just isFinished) = error "QueryComplete requires QueryTransformer and/or QueryCallback"
queryPtr adb alloc (Just transform) Nothing         Nothing           = error "QueryTransform requires QueryComplete"
queryPtr adb alloc Nothing          (Just callback) Nothing           = error "QueryCallback requires QueryComplete"
queryPtr adb alloc (Just transform) (Just callback) Nothing           = error "QueryTransform and QueryCallback requires QueryComplete"

query :: (Ptr ADB) -> QueryAllocator -> Maybe QueryTransformer -> Maybe (QueryCallback a) -> Maybe QueryComplete -> IO ADBQueryResults
query adb alloc transform callback isFinished =
  queryPtr adb alloc transform callback isFinished >>= peek

mkPointQuery :: ADBDatumPtr   -- query features
                -> ADBQuerySpecPtr
                -> IO ()
mkPointQuery = undefined

mkTrackQuery :: ADBDatumPtr    -- query features
                -> Int         -- number of tracks
                -> ADBQuerySpecPtr
                -> IO ()
mkTrackQuery = undefined

mkSequenceQuery :: ADBDatumPtr    -- query features
                   -> FeatureRate
                   -> Int         -- number of tracks
                   -> Seconds     -- sequence start
                   -> Seconds     -- sequence length
                   -> Maybe DistanceFlag
                   -> Maybe Double -- absolute power threshold
                   -> ADBQuerySpecPtr
                   -> IO ()
mkSequenceQuery datum secToFrames resultLen sqStart sqLen dist absThrsh qPtr = trace ("Evaluating mkSequenceQuery with datum @ " ++ (show datum))
  mkQuery datum (Just secToFrames) (Just sqStart) (Just sqLen) Nothing (Just perTrackFlag) (dist ||| euclideanNormedFlag) (Just 1) (Just resultLen) Nothing Nothing Nothing (absThrsh ||| 0) Nothing Nothing Nothing Nothing qPtr

execSequenceQuery :: (Ptr ADB)
                     -> ADBDatumPtr -- query features
                     -> FeatureRate
                     -> Int         -- number of tracks
                     -> Seconds     -- sequence start
                     -> Seconds     -- sequence length
                     -> Maybe DistanceFlag
                     -> Maybe Double -- absolute power threshold
                     -> IO ADBQueryResults
execSequenceQuery adb datum secToFrames resultLen sqStart sqLen dist absThrsh =
  querySinglePass adb (mkSequenceQuery datum secToFrames resultLen sqStart sqLen dist absThrsh)

transformSequenceQuery :: (ADBDatumPtr -> IO ADBDatumPtr)     -- query features
                          -> FeatureRate
                          -> FrameSize
                          -> (Int -> Int)                     -- number of tracks
                          -> (Seconds -> Seconds)             -- sequence start
                          -> (Seconds -> Seconds)             -- sequence length
                          -> (Maybe DistanceFlag -> Maybe DistanceFlag)
                          -> (Maybe Double -> Maybe Double)   -- absolute power threshold)
                          -> ADBQueryResultsPtr
                          -> QueryAllocator
                          -> ADBQuerySpecPtr
                          -> IO ()
transformSequenceQuery tDatum secToFrames framesToSec tResultLen tSqStart tSqLen tDist tAbsThrsh resPtr fromAlloc toPtr =
  withDetachedQueryPtr fromAlloc $ \fromPtr -> do
    q     <- peek fromPtr
    datum <- tDatum $ (queryid_datum . query_spec_qid) q
    let resultLen = tResultLen $ (query_parameters_ntracks . query_spec_params) q
        sqStart   = (withSeconds secToFrames framesToSec tSqStart ((queryid_sequence_start . query_spec_qid) q))
        sqLen     = (withSeconds secToFrames framesToSec tSqLen ((queryid_sequence_length . query_spec_qid) q))
        dist      = tDist      $ Just $ (query_parameters_distance . query_spec_params) q
        absThrsh  = tAbsThrsh  $ Just $ (query_refine_absolute_threshold . query_spec_refine) q
    mkSequenceQuery datum secToFrames resultLen (framesToSec sqStart) (framesToSec sqLen) dist absThrsh toPtr

mkNSequenceQuery :: ADBDatumPtr  -- query features
                    -> FeatureRate
                    -> Int         -- number of point nearest neighbours
                    -> Int         -- number of tracks
                    -> Seconds     -- sequence length
                    -> Maybe DistanceFlag
                    -> Maybe Double -- absolute power threshold
                    -> Int         -- query hop size
                    -> Int         -- instance hop size
                    -> ADBQuerySpecPtr
                    -> IO ()
mkNSequenceQuery datum secToFrames ptsNN resultLen sqLen dist absThrsh qHopSize iHopSize qPtr =
  mkQuery datum (Just secToFrames) (Just 0) (Just sqLen) (Just exhaustiveFlag) (Just perTrackFlag) (dist ||| euclideanNormedFlag) (Just ptsNN) (Just resultLen) Nothing Nothing Nothing (absThrsh ||| 0) Nothing Nothing (Just qHopSize) (Just iHopSize) qPtr

execNSequenceQuery :: (Ptr ADB)
                      -> ADBDatumPtr -- query features
                      -> FeatureRate
                      -> Int         -- number of point nearest neighbours
                      -> Int         -- number of tracks
                      -> Seconds     -- sequence length
                      -> Maybe DistanceFlag
                      -> Maybe Double -- absolute power threshold
                      -> Int         -- query hop size
                      -> Int         -- instance hop size
                      -> IO ADBQueryResults
execNSequenceQuery adb datum secToFrames ptsNN resultLen sqLen dist absThrsh qHopSize iHopSize =
  querySinglePass adb (mkNSequenceQuery datum secToFrames ptsNN resultLen sqLen dist absThrsh qHopSize iHopSize)

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
mkSequenceQueryDeltaNTracks secToFrames frameToSecs delta = transformSequenceQuery return secToFrames frameToSecs delta id id id id

mkSequenceQueryMutateDatum :: FeatureRate
                              -> FrameSize
                              -> (ADBDatumPtr -> IO ())
                              -> ADBQueryResultsPtr
                              -> QueryAllocator
                              -> ADBQuerySpecPtr
                              -> IO ()
mkSequenceQueryMutateDatum secToFrames frameToSecs mutate resPtr fromAlloc toPtr =
  trace "Evaluating mkSequenceQueryMutateDatum" withDetachedQueryPtr fromAlloc $ \fromPtr -> do
    putStrLn "Doing mkSequenceQueryMutateDatum"
    q <- peek fromPtr
    let datumPtr  = (queryid_datum . query_spec_qid) q
        resultLen = (query_parameters_ntracks . query_spec_params) q
        sqStart   = (queryid_sequence_start . query_spec_qid) q
        sqLen     = (queryid_sequence_length . query_spec_qid) q
        dist      = Just $ (query_parameters_distance . query_spec_params) q
        absThrsh  = Just $ (query_refine_absolute_threshold . query_spec_refine) q
    mutate datumPtr
    -- putStrLn $ "Making new sequence query with datum @" ++ (show datumPtr)
    -- d <- peek datumPtr
    -- putStrLn $ "Peeked datum @" ++ (show datumPtr)
    -- putStrLn $ show "Key: " ++ (show (datum_key d)) ++
    --   "; nVectors: " ++ (show (datum_nvectors d)) ++
    --   "; dim: " ++ (show (datum_dim d)) ++
    --   "; 240 features: " ++ (show (DV.take 240 (datum_data d)))-- ++
    --   --"; 100 times: " ++ (show (maybe (DV.fromList [0]) (\t -> (DV.take 100 t)) (datum_times d)))

    mkSequenceQuery datumPtr secToFrames resultLen (frameToSecs sqStart) (frameToSecs sqLen) dist absThrsh toPtr
    putStrLn "Finished mkSequenceQueryMutateDatum"
    putStrLn ""

rotateVector :: (DV.Storable a) => Int -> DV.Vector a -> DV.Vector a
rotateVector delta v = (DV.++) back front
  where (front, back) = DV.splitAt delta v

mapSlices :: (DV.Storable a) => (DV.Vector a -> DV.Vector a) -> Int -> DV.Vector a -> [DV.Vector a]
mapSlices f sliceLen values = mapSlice 0
  where mapSlice start
          | start + sliceLen <= (DV.length values) = (f (DV.slice start sliceLen values)) : mapSlice (start + sliceLen)
          | otherwise                              = []

rotateDatum :: Int -> ADBDatumPtr -> IO ()
rotateDatum delta datumPtr = do
  datum  <- peek datumPtr
  putStrLn $ "Rotating datum @" ++ (show datumPtr) ++ " by " ++ (show delta)
  let values    = datum_data datum
      rotValues = DV.concat $ mapSlices (rotateVector delta) (datum_dim datum) values
      rotDatum  = datum { datum_data = rotValues }

  poke datumPtr rotDatum
  putStrLn $ "Rotated datum @" ++ (show datumPtr) ++ " by " ++ (show delta)

mkSequenceQueryWithRotation :: ADBDatumPtr  -- query features
                               -> FeatureRate
                               -> FrameSize
                               -> Int          -- number of tracks
                               -> Seconds      -- sequence start
                               -> Seconds      -- sequence length
                               -> Maybe DistanceFlag
                               -> Maybe Double -- absolute power threshold
                               -> [Int]        -- rotations
                               -> (QueryAllocator, QueryTransformer, QueryComplete)
mkSequenceQueryWithRotation datum secToFrames frameToSecs resultLen sqStart sqLen dist absThrsh rotations = (alloc, transform, isFinished)
  where
    alloc            = mkSequenceQuery datum secToFrames resultLen sqStart sqLen dist absThrsh
    transform i r a  = trace ("Evaluating mkSequenceQueryWithRotation's transform function with i = " ++ (show i)) mkSequenceQueryMutateDatum secToFrames frameToSecs (rotateDatum (rotations!!(i - 1))) r a
    isFinished i _ r = return $ i > (length rotations)

execSequenceQueryWithRotation :: (Ptr ADB)
                               -> ADBDatumPtr  -- query features
                               -> FeatureRate
                               -> FrameSize
                               -> Int          -- number of tracks
                               -> Seconds      -- sequence start
                               -> Seconds      -- sequence length
                               -> Maybe DistanceFlag
                               -> Maybe Double -- absolute power threshold
                               -> [Int]        -- rotations
                               -> IO ADBQueryResults
execSequenceQueryWithRotation adb datum secToFrames frameToSecs resultLen sqStart sqLen dist absThrsh rotations =
  queryWithTransform adb alloc transform isFinished
  where (alloc, transform, isFinished) = mkSequenceQueryWithRotation datum secToFrames frameToSecs resultLen sqStart sqLen dist absThrsh rotations
