module Util.HttpUtil where

import           Control.Monad
import           Control.Monad.Trans(liftIO)
import           Data.ByteString (ByteString)
import           Snap.Core
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text.Lazy as T
import           Data.Int(Int64)

maxBodyLen = 1000000

readBody :: Snap String
readBody = do 
    liftM (T.unpack . E.decodeUtf8) (readRequestBody maxBodyLen)

writeResponse :: String -> Snap()
writeResponse = writeLBS . E.encodeUtf8 . T.pack

