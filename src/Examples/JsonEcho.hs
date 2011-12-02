{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Examples.JsonEcho where

import           Snap.Core
import           Data.Aeson.Generic as JSON
import           Util.HttpUtil
import           Data.Maybe(fromJust)
import           Data.Data
import           Data.Typeable

jsonEcho :: Snap()
jsonEcho = method POST $ do 
    reqBody <- readRequestBody maxBodyLen
    let hello = fromJust $ JSON.decode reqBody :: Hello
    writeLBS $ JSON.encode $ hello  

data Hello = Hello { message :: String } deriving (Data, Typeable, Show)
