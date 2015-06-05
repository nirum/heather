{-# LANGUAGE OverloadedStrings #-}
module Main where

import Heather -- the library for interfacing with the OpenWeatherMap API
import System.Console.ArgParser -- parse arguments with the argparser library

data MyArg = -- arguments consist of a query string and an optional zip code
  MyArg String Int deriving (Eq, Show)

myParser :: ParserSpec MyArg -- create the argument parser
myParser = MyArg
  `parsedBy` optPos "" "query" `Descr` "The desired query, either `current` for the current forecast or `forecast` for a list of predicted temperatures over the next three days"
  `andBy` optPos 94305 "zipcode" `Descr` "A 5-digit zip code (Default: 94305)"

getTemp :: Weather -> Float
getTemp (Weather t _ _ _ _ _ _ _ _) = t

app :: MyArg -> IO ()
app (MyArg q z)

  | q == "" = putStrLn "Forecast: `heather forecast`\nCurrent weather: `heather current`\n(add an additional argument to change the zip code, e.g. `heather forecast 27708`)"

  | q == "current" = do
      result <- getCurrent z -- get the current weather
      case result of
        Left ex -> print $ "Caught exception: " ++ show ex
        Right val -> print $ getTemp val

  | q == "forecast" = do
      result <- getForecast z -- get the current forecast
      case result of
        Left ex -> print $ "Caught exception: " ++ show ex
        Right val -> putStrLn $ collect val
          where collect (Forecast _ ws) = unwords $ map (show . getTemp) ws

  | otherwise = error "Invalid argument"

main :: IO ()
main = do
  interface <- mkApp myParser -- create our command line interface
  runApp interface app -- run the app
