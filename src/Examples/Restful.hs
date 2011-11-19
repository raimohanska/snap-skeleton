{-# LANGUAGE OverloadedStrings #-}

module Examples.Restful where

import           Control.Monad
import           Control.Monad.Trans(liftIO)
import           Data.ByteString (ByteString)
import           Snap.Core
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text.Lazy as T

-- TODO: implement restful service

bananas :: Snap()
bananas = do 
    reqBody <- readBody
    liftIO $ putStrLn $ "Received " ++ reqBody
    let reply = reqBody
    writeResponse reply
