{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Util.Rest where

import           Snap.Core
import           Util.HttpUtil
import           Prelude hiding (id, lookup)
restfulGet :: (String -> Snap()) -> Snap ()
restfulGet lookup = method GET $ do
    idPar <- getPar("id")
    case idPar of
     Just(id) -> lookup id
     Nothing -> notFound
