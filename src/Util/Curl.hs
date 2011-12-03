module Util.Curl where

import Network.HTTP
import Network.Stream(Result)

curlPostGetString :: String -> String -> IO (Int, String)
curlPostGetString url input = simpleHTTP (postRequestWithBody url "text/json" input) >>= convertResponse

curlGetGetString :: String -> IO (Int, String)
curlGetGetString url = simpleHTTP (getRequest url) >>= convertResponse

convertResponse resp = do
  body <- getResponseBody resp
  code <- getResponseCode resp
  return (code, body)

getResponseCode :: Result (Response ty) -> IO Int
getResponseCode (Left err) = fail $ show err
getResponseCode (Right r) = return $ (\(a, b, c) -> a*100+b*10+c) (rspCode r)
