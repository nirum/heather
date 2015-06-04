{-# LANGUAGE OverloadedStrings #-}
module Heather where

import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

{-
 - Loads data using the OpenWeatherMap API (http://openweathermap.org/api)
-}

-- city data type
data City = City { name :: String
                 , country :: String } deriving Show

instance FromJSON City where
  parseJSON (Object o) = City <$>
    o .: "name" <*>
    o .: "country"
  parseJSON _ = mzero

-- sample of weather data
data Weather =
  Weather { temperature :: Float            -- degrees Farenheit
          , pressure :: Float               -- hPa
          , humidity :: Float               -- %
          , windspeed :: Float              -- miles/hour
          , clouds :: Float                 -- %
          , category :: Text                -- short description
          , description :: Text             -- long description
          , time :: UTCTime                 -- POSIX timestamp
          , icon :: Text } deriving (Show)

-- container for city + a list of weather data
data Forecast = Forecast { city :: City, weather :: [Weather] } deriving Show

instance FromJSON Weather where
  parseJSON (Object o) = Weather <$>
    (main >>= (.: "temp")) <*>
    (main >>= (.: "pressure")) <*>
    (main >>= (.: "humidity")) <*>
    (wind >>= (.: "speed")) <*>
    (clouds >>= (.: "all")) <*>
    (desc >>= (.: "main")) <*>
    (desc >>= (.: "description")) <*>
    liftM parseTime (o .: "dt") <*>
    (desc >>= (.: "icon"))
      where main = (o .: "main")
            wind = (o .: "wind")
            clouds = (o .: "clouds")
            desc = (!! 0) <$> (o .: "weather")
            parseTime = posixSecondsToUTCTime . fromIntegral :: Int -> UTCTime

instance FromJSON Forecast where
  parseJSON (Object o) = do
    city <- parseJSON =<< (o .: "city")
    weather <- parseJSON =<< (o .: "list")
    return $ Forecast city weather
  parseJSON _ = mzero

url :: String -> String -> String
url t z = "http://api.openweathermap.org/data/2.5/" ++ t ++ "?q=" ++ z ++ "&units=imperial"

getForecast z = eitherDecode <$> ((simpleHttp . (url "forecast")) z) :: IO (Either String Forecast)
getCurrent z = eitherDecode <$> ((simpleHttp . (url "weather")) z) :: IO (Either String Weather)
