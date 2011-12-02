{-# LANGUAGE OverloadedStrings #-}

module Examples.Echo where

import           Control.Monad.Trans(liftIO)
import           Snap.Core
import           Util.HttpUtil

echo :: Snap ()
echo = method POST $ do 
    reqBody <- readBody
    liftIO $ putStrLn $ "Received " ++ reqBody
    let reply = reqBody
    writeResponse reply
