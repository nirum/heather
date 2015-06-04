{-# LANGUAGE OverloadedStrings #-}

module Main where

import Heather

-- Set your zipCode code here!
zipCode :: String
zipCode = "94305"

main :: IO ()
main = do
  result <- getCity zipCode
  case result of
    Left ex -> putStrLn $ "Caught exception: " ++ show ex
    Right val -> putStrLn $ "Weather for " ++ (getCity val) ++ ":"
      where getCity (Forecast (City city _) _) = city
