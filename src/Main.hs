module Main where

import           Snap.Http.Server
import           Control.Monad
import           Control.Monad.Trans(liftIO)
import           Data.ByteString (ByteString)
import           Snap.Core
import qualified Data.ByteString.Lazy.Char8 as L8

lol :: Snap()
lol = do 
    reqBody <- liftM L8.unpack getRequestBody
    liftIO $ putStrLn $ "Received " ++ reqBody
    let reply = "You got lolld"
    writeLBS $ L8.pack $ reply  

site :: Snap ()
site = route [ ("/", lol) ]

main :: IO ()
main = quickHttpServe site
