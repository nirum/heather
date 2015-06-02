{-# LANGUAGE OverloadedStrings #-}

module Heather where

import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)

-- Latitude / Longitude coordinates
data Coord =
  Coord { latitude :: Float
        , longitude :: Float } deriving (Show)

instance FromJSON Coord where
  parseJSON (Object v) = Coord <$>
    (c >>= (.: "lat")) <*>
    (c >>= (.: "lon"))
      where c = (v .: "coord")

instance ToJSON Coord where
  toJSON (Coord latitude longitude) =
    object [ "lat" .= latitude
           , "lon" .= longitude
           ]

-- Weather
data Weather =
  Weather { id :: Int
          , main :: Text
          , description :: Text
          , icon :: Text } deriving (Show)

instance FromJSON Weather where
  parseJSON (Object v) = Weather <$>
    (w >>= (.: "id")) <*>
    (w >>= (.: "main")) <*>
    (w >>= (.: "description")) <*>
    (w >>= (.: "icon"))
      where w = (v .: "weather")

-- Main
data Main =
  Main { temperature :: Float
       , pressure :: Float
       , humidity :: Float
       , low :: Float
       , high :: Float } deriving (Show)

instance FromJSON Main where
  parseJSON (Object v) = Main <$>
    (w >>= (.: "temp")) <*>
    (w >>= (.: "pressure")) <*>
    (w >>= (.: "humidity")) <*>
    (w >>= (.: "temp_min")) <*>
    (w >>= (.: "temp_max"))
      where w = (v .: "main")

url = "http://api.openweathermap.org/data/2.5/weather?q=94305&units=imperial" :: String
response = simpleHttp url

getCoord = eitherDecode <$> response :: IO (Either String Coord)
getWeather = eitherDecode <$> response :: IO (Either String Weather)
getMain = eitherDecode <$> response :: IO (Either String Main)
