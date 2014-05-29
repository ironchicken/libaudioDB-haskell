{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module ADB where

import Foreign
import Foreign.C.Types
import Foreign.C.String
-- import Foreign.Marshal.Array

#include "audioDB_API.h"

-- This is used by the hsc2hs pre-processor to calculate the alignment
-- for C structs
-- #let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

data ADB = ADB (Ptr ADB)
-- data AudioDB = AudioDB !(ForeignPtr ADB)

-- The ADB* ADTs are fairly literal mappings of each of the structs in
-- the API
data ADBDatum = ADBDatum {
  datum_nvectors :: CUInt,
  datum_dim      :: CUInt,
  datum_key      :: Ptr CChar,
  datum_data     :: Ptr CDouble,
  datum_power    :: Ptr CDouble,
  datum_times    :: Ptr CDouble } deriving (Eq, Show)

data ADBReference = ADBReference {
  reference_features :: Ptr CChar,
  reference_power    :: Ptr CChar,
  reference_key      :: Ptr CChar,
  reference_times    :: Ptr CChar } deriving (Eq, Show)

data ADBStatus = ADBStatus {
  status_numFiles         :: CUInt,
  status_dim              :: CUInt,
  status_dudCount         :: CUInt,
  status_nullCount        :: CUInt,
  status_flags            :: CUInt,
  status_length           :: CULong,
  status_data_region_size :: CULong } deriving (Eq, Show)

data ADBResult = ADBResult {
  result_qkey   :: Ptr CChar,
  result_ikey   :: Ptr CChar,
  result_qpos   :: CUInt,
  result_ipos   :: CUInt,
  result_dist   :: CDouble } deriving (Eq, Show)

data ADBKeyList = ADBKeyList {
  keylist_nkeys  :: CUInt,
  keylist_keys   :: Ptr (Ptr CChar) } deriving (Eq, Show)

data ADBQueryRefine = ADBQueryRefine {
  query_refine_flags              :: CUInt,
  query_refine_include            :: ADBKeyList,
  query_refine_exclude            :: ADBKeyList,
  query_refine_radius             :: CDouble,
  query_refine_absolute_threshold :: CDouble,
  query_refine_relative_threshold :: CDouble,
  query_refine_duration_ratio     :: CDouble,
  query_refine_qhopsize           :: CUInt,
  query_refine_ihopsize           :: CUInt } deriving (Eq, Show)

data ADBQueryParameters = ADBQueryParameters {
  query_parameters_accumulation  :: CUInt,
  query_parameters_distance      :: CUInt,
  query_parameters_npoints       :: CUInt,
  query_parameters_ntracks       :: CUInt } deriving (Eq, Show)

data ADBQueryResults = ADBQueryResults {
  query_results_nresults :: CUInt,
  query_results_results  :: Ptr ADBResult } deriving (Eq, Show)

data ADBQueryID = ADBQueryID {
  queryid_datum           :: Ptr ADBDatum,
  queryid_sequence_length :: CUInt,
  queryid_flags           :: CUInt,
  queryid_sequence_start  :: CUInt } deriving (Eq, Show)

data ADBQuerySpec = ADBQuerySpec {
  query_spec_qid      :: ADBQueryID,
  query_spec_params   :: ADBQueryParameters,
  query_spec_refine   :: ADBQueryRefine } deriving (Eq, Show)

data ADBTrackEntry = ADBTrackEntry {
  track_entry_nvectors :: CUInt,
  track_entry_key      :: Ptr CChar } deriving (Eq, Show)

data ADBLisztResults = ADBLisztResults {
  liszt_results_nresults :: CUInt,
  liszt_results_entries  :: Ptr ADBTrackEntry } deriving (Eq, Show)

-- Storable instances for all the structs allowing us to marshall them
-- in and out of the library
instance Storable ADBDatum where
  alignment _ = alignment (undefined :: CDouble)
  sizeOf _ = #{size adb_datum_t}

  peek d = do
    nv'    <- (#peek adb_datum_t, nvectors) d
    dim'   <- (#peek adb_datum_t, dim) d
    -- key'   <- peekCString (#{ptr adb_datum_t, key} d)
    key'   <- (#peek adb_datum_t, key) d
    data'  <- (#peek adb_datum_t, data) d
    power' <- (#peek adb_datum_t, power) d
    times' <- (#peek adb_datum_t, times) d
    return ADBDatum { datum_nvectors = nv',
                      datum_dim      = dim',
                      datum_key      = key',
                      datum_data     = data',
                      datum_power    = power',
                      datum_times    = times' }

  poke d (ADBDatum nv' dim' key' data' power' times') = do
    (#poke adb_datum_t, nvectors) d nv'
    (#poke adb_datum_t, dim) d dim'
    (#poke adb_datum_t, key) d key'
    (#poke adb_datum_t, data) d data'
    (#poke adb_datum_t, power) d power'
    (#poke adb_datum_t, times) d times'

instance Storable ADBReference where
  alignment _ = alignment (undefined :: CDouble)
  sizeOf _ = #{size adb_reference_t}

  peek r = do
    features' <- (#peek adb_reference_t, features) r
    power'    <- (#peek adb_reference_t, power) r
    key'      <- (#peek adb_reference_t, key) r
    times'    <- (#peek adb_reference_t, times) r
    return ADBReference { reference_features = features',
                          reference_power    = power',
                          reference_key      = key',
                          reference_times    = times' }

  poke r (ADBReference features' power' key' times') = do
    (#poke adb_reference_t, features) r features'
    (#poke adb_reference_t, power) r power'
    (#poke adb_reference_t, key) r key'
    (#poke adb_reference_t, times) r times'

instance Storable ADBStatus where
  alignment _ = alignment (undefined :: CDouble)
  sizeOf _ = #{size adb_status_t}

  peek s = do
    numFiles'         <- (#peek adb_status_t, numFiles) s
    dim'              <- (#peek adb_status_t, dim) s
    dudCount'         <- (#peek adb_status_t, dudCount) s
    nullCount'        <- (#peek adb_status_t, nullCount) s
    flags'            <- (#peek adb_status_t, flags) s
    length'           <- (#peek adb_status_t, length) s
    data_region_size' <- (#peek adb_status_t, data_region_size) s
    return ADBStatus { status_numFiles         = numFiles',
                       status_dim              = dim',
                       status_dudCount         = dudCount',
                       status_nullCount        = nullCount',
                       status_flags            = flags',
                       status_length           = length',
                       status_data_region_size = data_region_size' }

  poke s (ADBStatus numFiles' dim' dudCount' nullCount' flags' length' data_region_size') = do
    (#poke adb_status_t, numFiles) s numFiles'
    (#poke adb_status_t, dim) s dim'
    (#poke adb_status_t, dudCount) s dudCount'
    (#poke adb_status_t, nullCount) s nullCount'
    (#poke adb_status_t, flags) s flags'
    (#poke adb_status_t, length) s length'
    (#poke adb_status_t, data_region_size) s data_region_size'

instance Storable ADBResult where
  alignment _ = alignment (undefined :: CDouble)
  sizeOf _ = #{size adb_result_t}

  peek r = do
    qkey' <- (#peek adb_result_t, qkey) r
    ikey' <- (#peek adb_result_t, ikey) r
    qpos' <- (#peek adb_result_t, qpos) r
    ipos' <- (#peek adb_result_t, ipos) r
    dist' <- (#peek adb_result_t, dist) r
    return ADBResult { result_qkey = qkey',
                       result_ikey = ikey',
                       result_qpos = qpos',
                       result_ipos = ipos',
                       result_dist = dist' }

  poke r (ADBResult qkey' ikey' qpos' ipos' dist') = do
    (#poke adb_result_t, qkey) r qkey'
    (#poke adb_result_t, ikey) r ikey'
    (#poke adb_result_t, qpos) r qpos'
    (#poke adb_result_t, ipos) r ipos'
    (#poke adb_result_t, dist) r dist'

instance Storable ADBKeyList where
  alignment _ = alignment (undefined :: CDouble)
  sizeOf _ = #{size adb_keylist_t}

  peek kl = do
    nkeys' <- (#peek adb_keylist_t, nkeys) kl
    keys'  <- (#peek adb_keylist_t, keys) kl
    return ADBKeyList { keylist_nkeys = nkeys',
                        keylist_keys  = keys' }

  poke kl (ADBKeyList nkeys' keys') = do
    (#poke adb_keylist_t, nkeys) kl nkeys'
    (#poke adb_keylist_t, keys) kl keys'

instance Storable ADBQueryRefine where
  alignment _ = alignment (undefined :: CDouble)
  sizeOf _ = #{size adb_query_refine_t}

  peek qr = do
    flags'              <- (#peek adb_query_refine_t, flags) qr             
    include'            <- (#peek adb_query_refine_t, include) qr           
    exclude'            <- (#peek adb_query_refine_t, exclude) qr           
    radius'             <- (#peek adb_query_refine_t, radius) qr            
    absolute_threshold' <- (#peek adb_query_refine_t, absolute_threshold) qr
    relative_threshold' <- (#peek adb_query_refine_t, relative_threshold) qr
    duration_ratio'     <- (#peek adb_query_refine_t, duration_ratio) qr    
    qhopsize'           <- (#peek adb_query_refine_t, qhopsize) qr          
    ihopsize'           <- (#peek adb_query_refine_t, ihopsize) qr
    return ADBQueryRefine { query_refine_flags              = flags',
                            query_refine_include            = include',
                            query_refine_exclude            = exclude',
                            query_refine_radius             = radius',
                            query_refine_absolute_threshold = absolute_threshold',
                            query_refine_relative_threshold = relative_threshold',
                            query_refine_duration_ratio     = duration_ratio',
                            query_refine_qhopsize           = qhopsize',
                            query_refine_ihopsize           = ihopsize' }

  poke qr (ADBQueryRefine flags' include' exclude' radius' absolute_threshold' relative_threshold' duration_ratio' qhopsize' ihopsize') = do
    (#poke adb_query_refine_t, flags) qr flags'
    (#poke adb_query_refine_t, include) qr include'
    (#poke adb_query_refine_t, exclude) qr exclude'
    (#poke adb_query_refine_t, radius) qr radius'
    (#poke adb_query_refine_t, absolute_threshold) qr absolute_threshold'
    (#poke adb_query_refine_t, relative_threshold) qr relative_threshold'
    (#poke adb_query_refine_t, duration_ratio) qr duration_ratio'
    (#poke adb_query_refine_t, qhopsize) qr qhopsize'
    (#poke adb_query_refine_t, ihopsize) qr ihopsize'
                            
instance Storable ADBQueryParameters where
  alignment _ = alignment (undefined :: CDouble)
  sizeOf _ = #{size adb_query_parameters_t}

  peek qp = do
    accumulation' <- (#peek adb_query_parameters_t, accumulation) qp
    distance'     <- (#peek adb_query_parameters_t, distance) qp
    npoints'      <- (#peek adb_query_parameters_t, npoints) qp
    ntracks'      <- (#peek adb_query_parameters_t, ntracks) qp
    return ADBQueryParameters { query_parameters_accumulation = accumulation',
                                query_parameters_distance     = distance',
                                query_parameters_npoints      = npoints',
                                query_parameters_ntracks      = ntracks' }

  poke qp (ADBQueryParameters accumulation' distance' npoints' ntracks') = do
    (#poke adb_query_parameters_t, accumulation) qp accumulation'
    (#poke adb_query_parameters_t, distance) qp distance'
    (#poke adb_query_parameters_t, npoints) qp npoints'
    (#poke adb_query_parameters_t, ntracks) qp ntracks'
    
instance Storable ADBQueryResults where
  alignment _ = alignment (undefined :: CDouble)
  sizeOf _ = #{size adb_query_results_t}

  peek qr = do
    nresults' <- (#peek adb_query_results_t, nresults) qr
    results'  <- (#peek adb_query_results_t, results) qr
    return ADBQueryResults { query_results_nresults = nresults',
                             query_results_results  = results' }

  poke qr (ADBQueryResults nresults' results') = do
    (#poke adb_query_results_t, nresults) qr nresults'
    (#poke adb_query_results_t, results) qr results'

instance Storable ADBQueryID where
  alignment _ = alignment (undefined :: CDouble)
  sizeOf _ = #{size adb_query_id_t}

  peek qid = do
    datum'           <- (#peek adb_query_id_t, datum) qid
    sequence_length' <- (#peek adb_query_id_t, sequence_length) qid
    flags'           <- (#peek adb_query_id_t, flags) qid
    sequence_start'  <- (#peek adb_query_id_t, sequence_start) qid
    return ADBQueryID { queryid_datum           = datum',
                        queryid_sequence_length = sequence_length',
                        queryid_flags           = flags',
                        queryid_sequence_start  = sequence_start' }

  poke qid (ADBQueryID datum' sequence_length' flags' sequence_start') = do
    (#poke adb_query_id_t, datum) qid datum'
    (#poke adb_query_id_t, sequence_length) qid sequence_length'
    (#poke adb_query_id_t, flags) qid flags'
    (#poke adb_query_id_t, sequence_start) qid sequence_start'

instance Storable ADBQuerySpec where
  alignment _ = alignment (undefined :: CDouble)
  sizeOf _ = #{size adb_query_spec_t}

  peek qs = do
    qid'    <- (#peek adb_query_spec_t, qid) qs
    params' <- (#peek adb_query_spec_t, params) qs
    refine' <- (#peek adb_query_spec_t, refine) qs
    return ADBQuerySpec { query_spec_qid    = qid',
                          query_spec_params = params',
                          query_spec_refine = refine' }

  poke qs (ADBQuerySpec qid' params' refine') = do
    (#poke adb_query_spec_t, qid) qs qid'
    (#poke adb_query_spec_t, params) qs params'
    (#poke adb_query_spec_t, refine) qs refine'

instance Storable ADBTrackEntry where
  alignment _ = alignment (undefined :: CDouble)
  sizeOf _ = #{size adb_track_entry_t}

  peek te = do
    nvectors' <- (#peek adb_track_entry_t, nvectors) te
    key'      <- (#peek adb_track_entry_t, key) te
    return ADBTrackEntry { track_entry_nvectors = nvectors',
                           track_entry_key      = key' }

  poke te (ADBTrackEntry nvectors' key') = do
    (#poke adb_track_entry_t, nvectors) te nvectors'
    (#poke adb_track_entry_t, key) te key'

instance Storable ADBLisztResults where
  alignment _ = alignment (undefined :: CDouble)
  sizeOf _ = #{size adb_liszt_results_t}

  peek lr = do
    nresults' <- (#peek adb_liszt_results_t, nresults) lr
    entries'  <- (#peek adb_liszt_results_t, entries) lr
    return ADBLisztResults { liszt_results_nresults = nresults',
                             liszt_results_entries  = entries' }

  poke lr (ADBLisztResults nresults' entries') = do
    (#poke adb_liszt_results_t, nresults) lr nresults'
    (#poke adb_liszt_results_t, entries) lr entries'

-- Importing the ADB_ flags

-- class Flag a where
--   combineFlags :: [a] -> a
--   combineFlags = Flag . foldr ((.|.) . cInt) 0
  
--   cInt :: a -> CInt

newtype QueryIDFlag = QueryIDFlag { unQueryIDFlag :: CInt }
                    deriving (Eq, Show)
-- instance Flag QueryIDFlag where
--   cInt = unQueryIDFlag

#{enum QueryIDFlag, QueryIDFlag
 , exhaustiveFlag          = ADB_QID_FLAG_EXHAUSTIVE
 , allowFalsePositivesFlag = ADB_QID_FLAG_ALLOW_FALSE_POSITIVES
 }

combineQueryIDFlags :: [QueryIDFlag] -> QueryIDFlag
combineQueryIDFlags = QueryIDFlag . foldr ((.|.) . unQueryIDFlag) 0

-- data Accumulation = Database | PerTrack | OneToOne
newtype AccumulationFlag = AccumulationFlag { unAccumulationFlag :: CInt } deriving (Eq, Show)

#{enum AccumulationFlag, AccumulationFlag
 , databaseFlag = ADB_ACCUMULATION_DB
 , perTrackFlag = ADB_ACCUMULATION_PER_TRACK
 , oneToOneFlag = ADB_ACCUMULATION_ONE_TO_ONE
 }

combineAccumulationFlags :: [AccumulationFlag] -> AccumulationFlag
combineAccumulationFlags = AccumulationFlag . foldr ((.|.) . unAccumulationFlag) 0

-- data Distance = DotProduct | EuclideanNormed | Euclidean | KullbackLeiblerDivergence
newtype DistanceFlag = DistanceFlag { unDistanceFlag :: CInt } deriving (Eq, Show)

#{enum DistanceFlag, DistanceFlag
 , dotProductFlag                = ADB_DISTANCE_DOT_PRODUCT
 , euclideanNormedFlag           = ADB_DISTANCE_EUCLIDEAN_NORMED
 , euclideanFlag                 = ADB_DISTANCE_EUCLIDEAN
 , kullbackLeiblerDivergenceFlag = ADB_DISTANCE_KULLBACK_LEIBLER_DIVERGENCE
 }

combineDistanceFlags :: [DistanceFlag] -> DistanceFlag
combineDistanceFlags = DistanceFlag . foldr ((.|.) . unDistanceFlag) 0

newtype RefinementFlag = RefinementFlag { unRefinementFlag :: CInt } deriving (Eq, Show)

#{enum RefinementFlag, RefinementFlag
 , includeKeyListFlag    = ADB_REFINE_INCLUDE_KEYLIST
 , excludeKeyListFlag    = ADB_REFINE_EXCLUDE_KEYLIST
 , radiusFlag            = ADB_REFINE_RADIUS
 , absoluteThresholdFlag = ADB_REFINE_ABSOLUTE_THRESHOLD
 , relativeThresholdFlag = ADB_REFINE_RELATIVE_THRESHOLD
 , durationRatioFlag     = ADB_REFINE_DURATION_RATIO
 , hopSizeFlag           = ADB_REFINE_HOP_SIZE
 }

combineRefinementFlags :: [RefinementFlag] -> RefinementFlag
combineRefinementFlags = RefinementFlag . foldr ((.|.) . unRefinementFlag) 0

-- Importing the foreign functions

foreign import ccall unsafe "audioDB_API.h audiodb_open"
  audiodb_open :: CString -> CInt -> IO (Ptr ADB)

foreign import ccall unsafe "audioDB_API.h audiodb_create"
  audiodb_create :: CString -> CUInt -> CUInt -> CUInt -> IO (Ptr ADB)

foreign import ccall unsafe "audioDB_API.h audiodb_close"
  audiodb_close :: (Ptr ADB) -> IO ()

foreign import ccall unsafe "audioDB_API.h audiodb_l2norm"
  audiodb_l2norm :: (Ptr ADB) -> IO CInt

foreign import ccall unsafe "audioDB_API.h audiodb_power"
  audiodb_power :: (Ptr ADB) -> IO CInt

foreign import ccall unsafe "audioDB_API.h audiodb_insert_datum"
  audiodb_insert_datum :: (Ptr ADB) -> (Ptr ADBDatum) -> IO CInt

foreign import ccall unsafe "audioDB_API.h audiodb_insert_reference"
  audiodb_insert_reference :: (Ptr ADB) -> (Ptr ADBReference) -> IO CInt

foreign import ccall unsafe "audioDB_API.h audiodb_query_spec"
  audiodb_query_spec :: (Ptr ADB) -> (Ptr ADBQuerySpec) -> IO CInt

foreign import ccall unsafe "audioDB_API.h audiodb_query_free_results"
  audiodb_query_free_results :: (Ptr ADB) -> (Ptr ADBQuerySpec) -> (Ptr ADBQueryResults) -> IO CInt

foreign import ccall unsafe "audioDB_API.h audiodb_status"
  audiodb_status :: (Ptr ADB) -> (Ptr ADBStatus) -> IO CInt

foreign import ccall unsafe "audioDB_API.h audiodb_retrieve_datum"
  audiodb_retrieve_datum :: (Ptr ADB) -> CString -> (Ptr ADBDatum) -> IO CInt

foreign import ccall unsafe "audioDB_API.h audiodb_free_datum"
  audiodb_free_datum :: (Ptr ADB) -> (Ptr ADBDatum) -> IO CInt

foreign import ccall unsafe "audioDB_API.h audiodb_dump"
  audiodb_dump :: (Ptr ADB) -> CString -> IO CInt

foreign import ccall unsafe "audioDB_API.h audiodb_liszt"
  audiodb_liszt :: (Ptr ADB) -> IO (Ptr ADBLisztResults)

foreign import ccall unsafe "audioDB_API.h audiodb_liszt_free_results"
  audiodb_liszt_free_results :: (Ptr ADB) -> (Ptr ADBLisztResults) -> IO CInt

foreign import ccall unsafe "audioDB_API.h audiodb_sample_spec"
  audiodb_sample_spec :: (Ptr ADB) -> (Ptr ADBQuerySpec) -> IO (Ptr ADBQueryResults)

foreign import ccall unsafe "audioDB_API.h audiodb_insert"
  audiodb_insert :: (Ptr ADB) -> (Ptr ADBReference) -> IO CInt

foreign import ccall unsafe "audioDB_API.h audiodb_batchinsert"
  audiodb_batchinsert :: (Ptr ADB) -> (Ptr ADBReference) -> CUInt -> IO CInt
