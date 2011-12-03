module Util.HttpClient(post, get) where

import Network.HTTP
import Network.Stream(Result)
import Control.Monad

post :: String -> String -> IO (Int, String)
post url input = simpleHTTP (postRequestWithBody url "text/json" input) >>= convertResponse

get :: String -> IO (Int, String)
get url = simpleHTTP (getRequest url) >>= convertResponse

convertResponse resp = liftM2 (,) (getResponseCode resp) (getResponseBody resp)
  where getResponseCode (Left err) = fail $ show err
        getResponseCode (Right (Response (a, b, c) _ _ _)) = return $ a*100+b*10+c
