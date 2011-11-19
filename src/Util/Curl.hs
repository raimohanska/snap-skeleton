module Util.Curl where

import Network.Curl

curlPostGetString :: String -> String -> IO (Int, String)
curlPostGetString url input = withCurlDo $ do
  curl <- initialize
  resp <- do_curl_ curl url (CurlPostFields [input] : method_POST) :: IO CurlResponse
  return (respStatus resp, respBody resp)

curlGetGetString :: String -> IO (Int, String)
curlGetGetString url = withCurlDo $ do
  curl <- initialize
  resp <- do_curl_ curl url [] :: IO CurlResponse
  return (respStatus resp, respBody resp)


