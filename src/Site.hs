{-# LANGUAGE OverloadedStrings #-}

{-|

This is where all the routes and handlers are defined for your site. The
'app' function is the initializer that combines everything together and
is exported by this module.

-}

module Site
  ( app
  ) where

import           Control.Monad
import           Control.Monad.Trans
import           Data.ByteString (ByteString)
import           Snap.Core
import           Snap.Snaplet
import           Snap.Util.FileServe
import qualified Data.ByteString.Lazy.Char8 as L8
import           Application

lol = do 
    reqBody <- liftM L8.unpack getRequestBody
    liftIO $ putStrLn $ "Received " ++ reqBody
    let reply = "You got lolld"
    writeLBS $ L8.pack $ reply  

routes :: [(ByteString, Handler App App ())]
routes = [ ("/",            lol)
         , ("", serveDirectory "resources/static")
         ]

app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    addRoutes routes
    return $ App


