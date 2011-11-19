{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Examples.Restful where

import           Control.Monad
import           Control.Monad.Trans(liftIO)
import           Data.ByteString (ByteString)
import           Snap.Core
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text.Encoding as ES
import qualified Data.Text as TS
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text.Lazy as T
import           Util.HttpUtil
import           Data.Typeable
import           Data.Data
import           Text.JSON.Generic
import           Control.Applicative

data Banana = Banana { color :: String } deriving (Data, Typeable, Show)

bananas :: Snap()
bananas = newBanana <|> getBanana 

newBanana = method POST $ do 
    banana <- (liftM decodeJSON readBody) :: Snap Banana
    writeResponse $ encodeJSON $ (1 :: Int)


getPar :: String -> Snap (Maybe String)
getPar name = do
  p <- getParam $ ES.encodeUtf8 $ TS.pack $ name
  case p of
    Just val -> return $ Just $ TS.unpack $ ES.decodeUtf8 val
    Nothing -> return Nothing

getBanana = method GET $ do
    id <- getPar("id")
    case id of
     Just("1") -> writeResponse $ encodeJSON $ Banana "yellow"
     _         -> do modifyResponse $ setResponseStatus 404 "Not found"
                     writeBS "Not found"
 
