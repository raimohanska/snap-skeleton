module FunctionalSpec where

import Snap.Http.Server.Config
import Test.HUnit
import qualified Main as Main
import Control.Concurrent(forkIO, threadDelay, killThread)
import Util.Curl
import Network.Curl(curlGetString)
import Text.Regex.XMLSchema.String(match)
import Control.Exception(finally)
import Util.RegexEscape(escape)

functionalTests = TestList [
  postTest "Echo string" "/echo" "lol" $ Matching "l.*l"
  , postTest "Echo JSON" "/jsonecho" "{\"message\":\"hola\"}" $ Exactly "{\"message\":\"hola\"}"
  , postTest "POST restful Banana" "/banana" "{\"color\":\"yellow\"}" $ Exactly "\"1\""
  , getTest "GET restful Banana" "/banana/1" $ Exactly "{\"color\":\"yellow\"}" 
  , getTest "Unknown Banana not found - 404" "/banana/2" $ ReturnCode 404
  ]

testPort = 8001
rootUrl = "localhost:" ++ (show testPort) 

data ExpectedResult = Matching String | Exactly String | ReturnCode Int

postTest desc path request expected = 
  httpTest desc path (curlPostGetString (rootUrl ++ path) request) expected

getTest desc path expected = httpTest desc path (curlGetGetString (rootUrl ++ path)) expected

httpTest :: String -> String -> IO (Int, String) -> ExpectedResult -> Test
httpTest desc path request expected = TestLabel desc $ TestCase $ withTestServer $ do
    (code, body) <- request
    putStrLn $ "Got reply : " ++ body
    case expected of
      Matching pattern -> assertBool desc (match pattern (body))
      Exactly str -> assertEqual desc str body
      ReturnCode c -> assertEqual desc c code

withTestServer :: IO () -> IO ()
withTestServer task = do
    serverThread <- forkIO $ Main.serve (setPort testPort defaultConfig) 
    threadDelay $ toMicros 1000
    task `finally` (killThread serverThread)

toMicros = (*1000)
 
