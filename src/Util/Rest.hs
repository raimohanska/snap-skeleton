{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Util.Rest where

import           Control.Monad
import           Control.Monad.Trans(liftIO)
import           Snap.Core
import           Util.HttpUtil

restfulGet :: (String -> Snap()) -> Snap ()
restfulGet lookup = method GET $ do
    idPar <- getPar("id")
    case idPar of
     Just(id) -> lookup id
     Nothing -> notFound
