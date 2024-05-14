module Main where

import Data.HashSet (fromList)
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import Network.Wai.Handler.Warp (run)
import System.Directory (doesFileExist)

import ParseOpts (Config(..), execConfigParser)
import Server
import Types (Env(..))
import Cache (newCacheHandle)

main :: IO ()
main = do
  fileExists <- doesFileExist "config.yaml"
  cache' <- newCacheHandle
  case fileExists of
    True -> do 
      decodeResult <- decodeFileEither "config.yaml"
      case decodeResult of
        Left parseException -> fail $ prettyPrintParseException parseException
        Right config -> runServer cache' config 
    False -> do
      config <- execConfigParser
      runServer cache' config
  where
    runServer cacheHandle Config { apiRoot = apiRoot'
                                 , apiKey = apiKey'
                                 , ParseOpts.locations = locs
                                 , ParseOpts.measurementError = mError
                                 , port = port'
                                 , ParseOpts.updatePeriod = period
                                 } =
      run port' (app apiRoot' apiKey' $ MkEnv cacheHandle (fromList locs) period mError)