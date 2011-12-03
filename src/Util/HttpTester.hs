module Util.HttpTester where

import Snap.Http.Server.Config
import Test.HUnit
import Control.Concurrent(forkIO, threadDelay, killThread)
import qualified Util.HttpClient as HTTP
import Text.Regex.XMLSchema.String(match)
import Control.Exception(finally)
import Util.RegexEscape(escape)

wrapTest :: Wrapper -> Test -> Test
wrapTest wrapper (TestCase a) = TestCase $ wrapper a
wrapTest wrapper (TestList tests) = TestList $ map (wrapTest wrapper) tests
wrapTest wrapper (TestLabel label test) = TestLabel label $ wrapTest wrapper test

data ExpectedResult = Matching String | Exactly String | ReturnCode Int | All [ExpectedResult]

type Wrapper = IO () -> IO ()

post desc root path request expected = 
  httpTest desc (HTTP.post (root ++ path) request) expected

get desc root path expected = 
  httpTest desc (HTTP.get (root ++ path)) expected

httpTest :: String -> IO (Int, String) -> ExpectedResult -> Test
httpTest desc request expected = TestLabel desc $ TestCase $ do
    (code, body) <- request
    putStrLn $ "Got reply : " ++ body
    verify (code, body) expected
  where
      verify (code, body) expected = case expected of
        Matching pattern -> assertBool desc (match pattern (body))
        Exactly str -> assertEqual desc str body
        ReturnCode c -> assertEqual desc c code
        All checks -> mapM_ (verify (code, body)) checks

withForkedServer :: IO() -> Wrapper
withForkedServer server task = do
    serverThread <- forkIO server
    threadDelay $ toMicros 1000
    task `finally` (killThread serverThread)

toMicros = (*1000)

