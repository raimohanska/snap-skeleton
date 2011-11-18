module Util.ByteStringHelper where

import Data.ByteString as B
import Data.String

unpack :: B.ByteString -> String
unpack = read . show


pack :: String -> B.ByteString
pack = read . show
