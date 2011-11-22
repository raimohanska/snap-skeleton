{-# LANGUAGE DeriveDataTypeable #-}

module Examples.Restful where

import           Control.Monad
import           Control.Monad.Trans(liftIO)
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
    liftIO $ putStrLn $ "New banana: " ++ (show banana)
    writeResponse $ encodeJSON $ ("1" :: String) 


getBanana = restfulGet getBanana'    
  where getBanana' "1" = writeResponse $ encodeJSON $ Banana "yellow"
        getBanana' _   = notFound
