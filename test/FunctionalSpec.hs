module FunctionalSpec where

import Snap.Http.Server.Config
import Test.HUnit
import qualified Main as Main
import Control.Concurrent(forkIO, threadDelay, killThread)
import Util.Curl
import Network.Curl(curlGetString)
import Text.Regex.XMLSchema.String(match)
import Util.XmlMatch(clean)
import Control.Exception(finally)
import Util.RegexEscape(escape)

functionalTests = TestList [
  postTest "Echo string" "/echo" "lol" "l.*l"
  , postTest "Echo JSON" "/jsonecho" "{\"message\":\"hola\"}" $ escape "{\"message\":\"hola\"}"
  ]

testPort = 8001
rootUrl = "localhost:" ++ (show testPort) 

postTest :: String -> String -> String -> String -> Test
postTest desc path request pattern = 
  httpTest desc path (curlPostGetString (rootUrl ++ path) request) pattern

getTest desc path pattern = httpTest desc path (curlGetString (rootUrl ++ path) []  >>= return . snd) pattern

httpTest :: String -> String -> IO String -> String -> Test
httpTest desc path request pattern = TestLabel desc $ TestCase $ withTestServer $ do
    reply <- request
    putStrLn $ "Got reply : " ++ reply
    assertBool desc (match pattern (clean reply))

withTestServer :: IO () -> IO ()
withTestServer task = do
    serverThread <- forkIO $ Main.serve (setPort testPort defaultConfig) 
    threadDelay $ toMicros 1000
    task `finally` (killThread serverThread)

toMicros = (*1000)
 
