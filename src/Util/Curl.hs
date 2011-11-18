module Util.Curl where

import Network.Curl

curlPostGetString :: String -> String -> IO String
curlPostGetString url input = withCurlDo $ do
  curl <- initialize
  resp <- do_curl_ curl url (CurlPostFields [input] : method_POST) :: IO CurlResponse
  let body = respBody resp
  return body


