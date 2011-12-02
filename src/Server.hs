{-# LANGUAGE OverloadedStrings #-}

module Server where

import           Snap.Http.Server
import           Snap.Core
import           Examples.Echo
import           Examples.JsonEcho
import           Examples.Restful

main :: IO ()
main = serve defaultConfig

serve :: Config Snap a -> IO()
serve config = httpServe config $ route [ 
  ("/echo", echo)
  ,("/jsonecho", jsonEcho)
  ,("/banana", bananas)
  ,("/banana/:id", bananas) ] 

