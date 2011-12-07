module FunctionalSpec where

import Snap.Http.Server.Config
import Test.HUnit
import Server
import Util.HttpTester

functionalTests = wrapTest withTestServer $ TestList [
  post "Echo string" url "/echo" "lol" $ Matching "l.*l"
  , post "Echo JSON" url "/jsonecho" "{\"message\":\"hola\"}" $ Exactly "{\"message\":\"hola\"}"
  , get "Echo JSON with GET = 404" url "/jsonecho" $ ReturnCode 404
  , post "POST restful Banana" url "/banana" "{\"color\":\"yellow\"}" $ Exactly "\"1\""
  , post "POST rotten Banana" url "/banana" "{wtf?}" $ All $ [ReturnCode 500, Matching ".*rotten.*"]
  , get "GET restful Banana" url "/banana/1" $ Exactly "{\"color\":\"yellow\"}" 
  , get "Unknown Banana not found - 404" url "/banana/2" $ ReturnCode 404
  ]

port = 8001
url= "http://localhost:" ++ (show port) 

withTestServer = withForkedServer $ Server.serve (setPort port defaultConfig) 
