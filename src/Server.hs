{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Control.Applicative (liftA2)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Reader.Class (MonadReader, ask)
import Data.HashSet qualified as HS
import Data.String (fromString)
import Lens.Micro ((^.), (&), (.~))
import Network.Wreq (getWith, asJSON, responseBody, defaults, param)
import Servant

import Cache qualified
import Types

type API = "weather" 
        :> QueryParam "lat" Double
        :> QueryParam "lon" Double
        :> Get '[JSON] Weather

type APIRoot = String
type APIKey = String

userAPI :: Proxy API
userAPI = Proxy

weather :: ( HasCache env, HasUpdatePeriod env, HasLocs env, HasMError env
           , MonadReader env m, MonadIO m
           )
        => APIRoot -> APIKey -> Maybe Double -> Maybe Double -> m Weather
weather apiRoot apiKey lat lon = case liftA2 (,) lat lon of
  Nothing -> error "Latitude and longitude needed"
  Just (lat', lon') -> do
    env <- ask
    let
      location      = MkLocation lat' lon'
      cache'        = getCache env
      updatePeriod' = getUpdatePeriod env
      locations'    = getLocs env
      mError        = getMError env

    if HS.member (roundLocation mError location) locations'
      then liftIO $ Cache.cached cache' updatePeriod' location apiCall 
      else liftIO apiCall 
    where
      apiCall = do
        response <- asJSON =<< getWith opts apiRoot
        return $ response ^. responseBody

      opts = defaults & param "lat"   .~ [fromString $ show lat']
                      & param "lon"   .~ [fromString $ show lon']
                      & param "appid" .~ [fromString apiKey]
      roundLocation err (MkLocation la lo) = MkLocation 
        (roundToErr la err) (roundToErr lo err)
        where
          roundToErr :: Double -> Int -> Double
          roundToErr x n = fromInteger (round $ x * (10^n)) / (10.0 ^^ n)

weatherServer :: APIRoot -> APIKey -> ServerT API App
weatherServer = weather

appToHandler :: Env -> App a -> Handler a
appToHandler env reader = liftIO $ runReaderT reader env

server :: APIRoot -> APIKey -> Env -> Server API
server apiRoot apiKey env = 
  hoistServer userAPI (appToHandler env) (weatherServer apiRoot apiKey) 

app :: APIRoot -> APIKey -> Env -> Application
app apiRoot apiKey env = serve userAPI (server apiRoot apiKey env)
