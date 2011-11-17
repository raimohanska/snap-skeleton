module Main where

import           Snap.Http.Server
import           Control.Monad
import           Control.Monad.Trans(liftIO)
import           Data.ByteString (ByteString)
import           Snap.Core
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text.Lazy as T

lol :: Snap()
lol = do 
    reqBody <- liftM (T.unpack . E.decodeUtf8) getRequestBody
    liftIO $ putStrLn $ "Received " ++ reqBody
    let reply = "You got lolld"
    writeLBS $ (E.encodeUtf8 . T.pack) $ reply  

site :: Snap ()
site = route [ ("/", lol) ]

main :: IO ()
main = quickHttpServe site
