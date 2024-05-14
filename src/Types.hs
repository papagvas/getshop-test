{-# LANGUAGE DeriveGeneric #-}

module Types where

import Control.Monad.Reader (ReaderT)
import Data.Aeson
import Data.Hashable (Hashable)
import Data.HashSet (HashSet)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)
import Data.Vector (head)
import GHC.Generics
import Prelude hiding (head)

import Cache (CacheHandle) 

data Location = MkLocation !Double !Double 
  deriving (Eq, Show, Ord, Generic)

instance Hashable Location
instance FromJSON Location

data Weather = MkWeather { degrees     :: !Double
                         , feelsLike   :: !Double
                         , description :: !Text
                         } deriving (Eq, Show, Generic)

instance FromJSON Weather where
  parseJSON = withObject "Weather" $ \obj -> do
    degrees' <- obj .: "main" >>= (.: "temp") 
    feelsLike' <- obj .: "main" >>= (.: "feels_like")
    description' <- obj .: "weather" >>= 
      \weatherArray -> head weatherArray .: "description"
    return $ MkWeather degrees' feelsLike' description'

instance ToJSON Weather

type App = ReaderT Env IO

data Env = MkEnv { cache            :: !(CacheHandle Location Weather)
                 , locations        :: !(HashSet Location)
                 , updatePeriod     :: !NominalDiffTime
                 , measurementError :: !Int
                 }

class HasCache a where
  getCache :: a -> CacheHandle Location Weather

instance HasCache Env where
  getCache = cache

class HasUpdatePeriod a where
  getUpdatePeriod :: a -> NominalDiffTime

instance HasUpdatePeriod Env where
  getUpdatePeriod = updatePeriod

class HasLocs a where
  getLocs :: a -> HashSet Location

instance HasLocs Env where
  getLocs = locations

class HasMError a where
  getMError :: a -> Int 

instance HasMError Env where
  getMError = measurementError
