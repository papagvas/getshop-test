module ParseOpts where

import Data.Aeson
-- import Data.Aeson.Types qualified as Aeson
import Data.Char (isDigit)
import Data.Time.Clock (NominalDiffTime)
import Options.Applicative hiding (many)
import Text.ParserCombinators.ReadP qualified as RP
import Text.Read (readMaybe)

import Types (Location, Location(..))

type Port = Int

data Config = Config
  { port :: Port
  , locations :: [Location]
  , updatePeriod :: NominalDiffTime
  , measurementError :: Int
  , apiRoot :: String
  , apiKey  :: String
  }

instance FromJSON Config where
  parseJSON = withObject "Config" $ \obj -> do
    port'   <- obj .: "port"
    locs    <- obj .: "locations"
    updateP <- obj .: "updatePeriod" >>= parseMError
    mError  <- obj .: "measurementError"
    apiRoot' <- obj .: "apiRoot"
    apiKey'  <- obj .: "apiKey"
    return $ Config port' locs updateP mError apiRoot' apiKey'
    where
      parseMError s = case s of
        [] -> fail "update period parsing failed"
        _  -> case readMaybe (init s) of 
          Nothing  -> fail "update period parsing failed"
          Just int -> case last s of
            'h' -> return . fromInteger $ 3600 * int
            'm' -> return . fromInteger $ 60 * int
            's' -> return $ fromInteger int
            _   -> fail "update period parsing failed"

configParser :: Parser Config
configParser = Config
  <$> portParser
  <*> locationsParser
  <*> periodParser
  <*> errorParser
  <*> apiRootParser
  <*> apiKeyParser

execConfigParser :: IO Config
execConfigParser = execParser opts
  where
    opts = info (configParser <**> helper) (fullDesc <> progDesc "Config parser")

readPToReadM :: RP.ReadP a -> ReadM a
readPToReadM = maybeReader . fmap listToMaybe . fmap fmap fmap fst . RP.readP_to_S
  where
    listToMaybe list = case list of
      [] -> Nothing
      xs -> Just $ last xs

portParser :: Parser Port
portParser = option auto 
  (long "port" <> short 'p' <> metavar "PORT" <> help "Port number")

locationsParser :: Parser [Location]
locationsParser = option
  (readPToReadM $ RP.sepBy parsePair (RP.char ' '))
  (long "locations" <> short 'l' <> metavar "LOCATIONS" <> help "Locations to cache")
  where
    parsePair = RP.between (RP.char '(') (RP.char ')') parseLocation
    parseLocation = do
      lat <- parseFloat 
      RP.skipSpaces
      RP.char ','
      RP.skipSpaces
      lon <- parseFloat
      return $ MkLocation lat lon
    parseFloat = do
      sign <- RP.munch (=='-')
      intPart <- digit
      RP.char '.'
      fracPart <- digit
      return $ read (sign ++ intPart ++ "." ++ fracPart)
    digit = RP.munch1 isDigit

periodParser :: Parser NominalDiffTime
periodParser = option
  (readPToReadM parseDiffTime)
  (long "period" <> short 'u' <> metavar "PERIOD" <> help "Update period")
  where
    parseDiffTime = do
      intPart <- RP.munch1 isDigit
      dim <- RP.choice $ RP.satisfy . (==) <$> ['h', 's', 'm']
      case dim of
        'h' -> return . fromInteger $ read intPart * 3600
        'm' -> return . fromInteger $ read intPart * 60
        's' -> return . fromInteger $ read intPart
        _   -> error "impossible"

errorParser :: Parser Int
errorParser = option
  (readPToReadM $ read <$> RP.munch1 isDigit)
  (long "error" <> short 'e' <> metavar "ERROR" <> help "Measurement error")

apiRootParser :: Parser String
apiRootParser = option auto
  (  long "apiRoot" 
  <> short 'r' 
  <> metavar "APIROOT" 
  <> value "https://api.openweathermap.org/data/2.5/weather"
  )

apiKeyParser :: Parser String
apiKeyParser = option auto
  (  long "apiKey" 
  <> short 'k' 
  <> metavar "APIKEY" 
  )
