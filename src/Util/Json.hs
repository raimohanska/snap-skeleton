{-# LANGUAGE NoMonomorphismRestriction #-}

module Util.Json where

import           Snap.Core
import           Data.Aeson.Generic as JSON
import           Data.Maybe(fromJust)
import           Util.HttpUtil

readBodyJson = readRequestBody maxBodyLen >>= return . fromJust . JSON.decode
