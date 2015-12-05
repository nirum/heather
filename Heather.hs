{-# LANGUAGE OverloadedStrings #-}
module Heather where

import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import Network.HTTP.Conduit (simpleHttp)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

-- put your API key here
apikey :: String
apikey = "API KEY GOES HERE"

-- the OpenWeatherMap API url
url :: (Show a) => String -> String -> a -> String
url q appid z = "http://api.openweathermap.org/data/2.5/" ++ q ++ "?q=" ++ (show z) ++ "&units=imperial&appid=" ++ appid
 
-- city datatype
data City = City { name :: String
                 , country :: String } deriving Show

-- cities contain "name" and "country" fields in the JSON schema
instance FromJSON City where
  parseJSON (Object o) = City <$>
    o .: "name" <*>
    o .: "country"
  parseJSON _ = mzero

-- weather datatype
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

-- parse the weather JSON schema
instance FromJSON Weather where
  parseJSON (Object o) = Weather <$>
    (main >>= (.: "temp")) <*>
    (main >>= (.: "pressure")) <*>
    (main >>= (.: "humidity")) <*>
    (wind >>= (.: "speed")) <*>
    (cloud >>= (.: "all")) <*>
    (desc >>= (.: "main")) <*>
    (desc >>= (.: "description")) <*>
    liftM parseTime (o .: "dt") <*>
    (desc >>= (.: "icon"))
      where main = (o .: "main")
            wind = (o .: "wind")
            cloud = (o .: "clouds")
            desc = (!! 0) <$> (o .: "weather")
            parseTime = posixSecondsToUTCTime . fromIntegral :: Int -> UTCTime

-- a Forecast is a container for city + a list of weather data
data Forecast = Forecast { city :: City, weather :: [Weather] } deriving Show

-- pare the full JSON response
instance FromJSON Forecast where
  parseJSON (Object o) = do
    c <- parseJSON =<< (o .: "city")
    w <- parseJSON =<< (o .: "list")
    return $ Forecast c w
  parseJSON _ = mzero

-- main functions for querying the API and parsing the JSON response
getForecast :: (Show a) => a -> IO (Either String Forecast)
getForecast z = eitherDecode <$> ((simpleHttp . (url "forecast" apikey)) z)

getCurrent :: (Show a) => a -> IO (Either String Weather)
getCurrent z = eitherDecode <$> ((simpleHttp . (url "weather" apikey)) z)
