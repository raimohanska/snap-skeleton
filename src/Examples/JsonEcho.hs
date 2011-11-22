{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Examples.JsonEcho where

import           Snap.Core
import           Text.JSON.Generic
import           Util.HttpUtil

jsonEcho :: Snap()
jsonEcho = do 
    reqBody <- readBody
    let hello = decodeJSON reqBody :: Hello
    writeResponse $ encodeJSON $ hello  

data Hello = Hello { message :: String } deriving (Data, Typeable, Show)
