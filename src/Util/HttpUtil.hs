{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, ScopedTypeVariables #-}

module Util.HttpUtil where

import           Control.Monad
import           Snap.Core
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text.Lazy as T
import qualified Data.Text.Encoding as ES
import qualified Data.Text as TS
import           Control.Exception.Base
import qualified Control.Monad.CatchIO as MCIO

maxBodyLen = 1000000

readBody :: Snap String
readBody = do 
    liftM (T.unpack . E.decodeUtf8) (readRequestBody maxBodyLen)

writeResponse :: String -> Snap()
writeResponse = writeLBS . E.encodeUtf8 . T.pack

getPar :: String -> Snap (Maybe String)
getPar name = do
  p <- getParam $ ES.encodeUtf8 $ TS.pack $ name
  return $ fmap (TS.unpack . ES.decodeUtf8) p

notFound :: Snap ()
notFound = writeErrorResponse 404 "Not found"

writeErrorResponse :: Int -> String -> Snap()
writeErrorResponse code message = do modifyResponse $ setResponseStatus code $ ES.encodeUtf8 $ TS.pack $ message
                                     writeResponse message

catchError :: String -> Snap () -> Snap () 
catchError msg action = action `MCIO.catch` \(e::SomeException) -> writeErrorResponse 500 msg
