{-# LANGUAGE DeriveDataTypeable #-}

module Examples.Restful where

import           Control.Monad
import           Control.Monad.Trans(liftIO)
import           Data.ByteString (ByteString)
import           Snap.Core
import           Data.Typeable
import           Data.Data
import           Text.JSON.Generic
import           Control.Applicative
import           Util.HttpUtil
import           Util.Rest

data Banana = Banana { color :: String } deriving (Data, Typeable, Show)

bananas :: Snap()
bananas = newBanana <|> getBanana 

newBanana = method POST $ do 
    banana <- (liftM decodeJSON readBody) :: Snap Banana
    let bananaId = "1"
    writeResponse $ encodeJSON $ bananaId 


getBanana = restfulGet getBanana'    
  where getBanana' "1" = writeResponse $ encodeJSON $ Banana "yellow"
        getBanana' _   = notFound
