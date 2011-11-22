module Util.HttpTester where

import Snap.Http.Server.Config
import Test.HUnit
import qualified Main as Main
import Control.Concurrent(forkIO, threadDelay, killThread)
import Util.Curl
import Network.Curl(curlGetString)
import Text.Regex.XMLSchema.String(match)
import Control.Exception(finally)
import Util.RegexEscape(escape)

wrapTest :: Wrapper -> Test -> Test
wrapTest wrapper (TestCase a) = TestCase $ wrapper a
wrapTest wrapper (TestList tests) = TestList $ map (wrapTest wrapper) tests
wrapTest wrapper (TestLabel label test) = TestLabel label $ wrapTest wrapper test

data ExpectedResult = Matching String | Exactly String | ReturnCode Int

type Wrapper = IO () -> IO ()

post desc root path request expected = 
  httpTest desc (curlPostGetString (root ++ path) request) expected

get desc root path expected = 
  httpTest desc (curlGetGetString (root ++ path)) expected

httpTest :: String -> IO (Int, String) -> ExpectedResult -> Test
httpTest desc request expected = TestLabel desc $ TestCase $ do
    (code, body) <- request
    putStrLn $ "Got reply : " ++ body
    case expected of
      Matching pattern -> assertBool desc (match pattern (body))
      Exactly str -> assertEqual desc str body
      ReturnCode c -> assertEqual desc c code

withForkedServer :: IO() -> Wrapper
withForkedServer server task = do
    serverThread <- forkIO server
    threadDelay $ toMicros 1000
    task `finally` (killThread serverThread)

toMicros = (*1000)

