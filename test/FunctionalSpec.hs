{-# LANGUAGE QuasiQuotes #-}
module FunctionalSpec where

import Snap.Http.Server.Config
import Test.HUnit
import Server
import Util.HttpTester
import Data.Aeson.QQ
import Data.Aeson.Types
import Data.Text

functionalTests = wrapTest withTestServer $ TestList [
  post "Echo string" url "/echo" "lol" $ Matching "l.*l"
  , postJson "Echo JSON" url "/jsonecho" [aesonQQ|{message:"hola"}|] $ Json [aesonQQ| {message:"hola"} |]
  , get "Echo JSON with GET = 404" url "/jsonecho" $ ReturnCode 404
  , postJson "POST restful Banana" url "/banana" [aesonQQ|{color:"yellow"}|] $ Exactly "\"1\""
  , post "POST rotten Banana" url "/banana" "{wtf?}" $ All $ [ReturnCode 500, Matching ".*rotten.*"]
  , get "GET restful Banana" url "/banana/1" $ Json [aesonQQ|{color:"yellow"}|] 
  , get "Unknown Banana not found - 404" url "/banana/2" $ ReturnCode 404
  ]

port = 8001
url= "http://localhost:" ++ (show port) 

withTestServer = withForkedServer $ Server.serve (setPort port defaultConfig) 
