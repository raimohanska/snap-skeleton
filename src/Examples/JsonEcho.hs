{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Examples.JsonEcho where

import           Control.Monad
import           Control.Monad.Trans(liftIO)
import           Data.ByteString (ByteString)
import           Snap.Core
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text.Lazy as T
import           Text.JSON.Generic

jsonEcho :: Snap()
jsonEcho = do 
    reqBody <- liftM (T.unpack . E.decodeUtf8) getRequestBody
    let hello = decodeJSON reqBody :: Hello
    writeLBS $ (E.encodeUtf8 . T.pack) $ encodeJSON $ hello  

data Hello = Hello { message :: String } deriving (Data, Typeable, Show)
