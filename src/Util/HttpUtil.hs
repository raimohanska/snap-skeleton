{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Util.HttpUtil where

import           Control.Monad
import           Control.Monad.Trans(liftIO)
import           Data.ByteString (ByteString)
import           Snap.Core
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text.Lazy as T
import qualified Data.Text.Encoding as ES
import qualified Data.Text as TS
import           Data.Int(Int64)

maxBodyLen = 1000000

readBody :: Snap String
readBody = do 
    liftM (T.unpack . E.decodeUtf8) (readRequestBody maxBodyLen)

writeResponse :: String -> Snap()
writeResponse = writeLBS . E.encodeUtf8 . T.pack

getPar :: String -> Snap (Maybe String)
getPar name = do
  p <- getParam $ ES.encodeUtf8 $ TS.pack $ name
  case p of
    Just val -> return $ Just $ TS.unpack $ ES.decodeUtf8 val
    Nothing -> return Nothing

notFound :: Snap ()
notFound = do modifyResponse $ setResponseStatus 404 "Not found"
              writeBS "Not found"
