{-# LANGUAGE OverloadedStrings #-}

module Main where

import Heather
import System.Console.ArgParser

{--- Set your zipCode code here!-}
{-zipCode :: String-}
{-zipCode = "94305"-}

{-main :: IO ()-}
{-main = do-}
  {-result <- getForecast zipCode-}
  {-case result of-}
    {-Left ex -> putStrLn $ "Caught exception: " ++ show ex-}
    {-Right val -> putStrLn $ "Weather for " ++ (getForecast val) ++ ":"-}
      {-where getForecast (Forecast (City city _) _) = city-}

data MyArg = MyArg String deriving (Eq, Show)
myParser :: ParserSpec MyArg
myParser = MyArg `parsedBy` reqPos "query"

app :: MyArg -> IO ()
app (MyArg z)
  | z == "forecast" = print "asked for Forecast"
  | z == "current" = print "asked for Current"
  | otherwise = error "Invalid argument"

main :: IO ()
main = do
  interface <- mkApp myParser
  runApp interface app
