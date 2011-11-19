module Util.Curl where

import Network.Curl

curlPostGetString :: String -> String -> IO (Int, String)
curlPostGetString url input = curlWithArgs (CurlPostFields [input] : method_POST) url

curlGetGetString :: String -> IO (Int, String)
curlGetGetString url = curlWithArgs [] url

curlWithArgs args url = withCurlDo $ do
  curl <- initialize
  resp <- do_curl_ curl url args :: IO CurlResponse
  return (respStatus resp, respBody resp)
