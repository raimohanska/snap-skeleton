{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Examples.JsonEcho where

import           Snap.Core
import           Data.Aeson.Generic as JSON
import           Util.HttpUtil
import           Data.Maybe(fromJust)
import           Data.Data
import           Data.Typeable
import           Util.Json

jsonEcho :: Snap ()
jsonEcho = method POST $ do 
    hello <- readBodyJson :: Snap Hello
    writeLBS $ JSON.encode $ hello  

data Hello = Hello { message :: String } deriving (Data, Typeable, Show)
