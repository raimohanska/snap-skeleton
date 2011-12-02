{-# LANGUAGE DeriveDataTypeable #-}

module Examples.Restful where

import           Control.Monad
import           Control.Monad.Trans(liftIO)
import           Snap.Core
import           Data.Typeable
import           Data.Data
import           Data.Aeson.Generic as JSON
import           Data.Maybe(fromJust)
import           Control.Applicative
import           Util.HttpUtil
import           Util.Rest

data Banana = Banana { color :: String } deriving (Data, Typeable, Show)

bananas :: Snap ()
bananas = newBanana <|> getBanana 

newBanana = method POST $ catchError "Banana is rotten" $ do 
    banana <- readRequestBody maxBodyLen >>= return . fromJust . JSON.decode :: Snap Banana
    liftIO $ putStrLn $ "New banana: " ++ (show banana)
    writeLBS $ JSON.encode $ ("1" :: String) 


getBanana = restfulGet getBanana'    
  where getBanana' "1" = writeLBS $ JSON.encode $ Banana "yellow"
        getBanana' _   = notFound
