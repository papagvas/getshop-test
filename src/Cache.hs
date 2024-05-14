module Cache (CacheHandle, newCacheHandle, cached) where

import Data.Hashable (Hashable)
import Data.HashPSQ (HashPSQ)
import Data.HashPSQ qualified as PSQ
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Data.Time.Clock (UTCTime, NominalDiffTime, getCurrentTime, diffUTCTime)

type Priority = UTCTime

newtype Cache k v = MkCache { cQueue :: HashPSQ k Priority v } 
  deriving (Eq, Show)

empty :: Cache k v
empty = MkCache PSQ.empty

lookup :: (Ord k, Hashable k) => k -> Cache k v -> Maybe (Priority, v)
lookup k (MkCache q) = PSQ.lookup k q

insert :: (Hashable k, Ord k) => k -> Priority -> v -> Cache k v -> Cache k v
insert k prio v cache@(MkCache q) = cache
  { cQueue = snd $ PSQ.insertView k prio v q }

newtype CacheHandle k v = MkCacheHandle (IORef (Cache k v))

newCacheHandle :: IO (CacheHandle k v)
newCacheHandle = 
  MkCacheHandle <$> newIORef empty

cached :: (Hashable k, Ord k) 
       => CacheHandle k v -> NominalDiffTime -> k -> IO v -> IO v
cached (MkCacheHandle ref) updatePeriod key io = do
  cache <- readIORef ref
  case Cache.lookup key cache of
    Nothing -> do
      v <- io
      time <- getCurrentTime
      atomicModifyIORef' ref $ \c -> (insert key time v c, ())
      return v
    Just (prio, value) -> do
      time <- getCurrentTime
      if diffUTCTime time prio < updatePeriod
        then return value
        else do
          v <- io
          time' <- getCurrentTime
          atomicModifyIORef' ref $ \c -> (insert key time' v c, ())
          return v