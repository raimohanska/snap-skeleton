module FunctionalSpec where

import Snap.Http.Server.Config
import Test.HUnit
import qualified Main as Main
import HttpTester

functionalTests = wrapTest withTestServer $ TestList [
  post "Echo string" url "/echo" "lol" $ Matching "l.*l"
  , post "Echo JSON" url "/jsonecho" "{\"message\":\"hola\"}" $ Exactly "{\"message\":\"hola\"}"
  , post "POST restful Banana" url "/banana" "{\"color\":\"yellow\"}" $ Exactly "\"1\""
  , get "GET restful Banana" url "/banana/1" $ Exactly "{\"color\":\"yellow\"}" 
  , get "Unknown Banana not found - 404" url "/banana/2" $ ReturnCode 404
  ]

port = 8001
url= "localhost:" ++ (show port) 

withTestServer = withForkedServer $ Main.serve (setPort port defaultConfig) 
