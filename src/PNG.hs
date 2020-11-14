{-# LANGUAGE RecordWildCards #-}
module PNG where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

data PNG = PNG
  { header :: ByteString,
    body :: ByteString
  } deriving (Show, Eq)

decodePNG :: ByteString -> PNG
decodePNG bs = PNG{..}
  where (header, body) = BS.splitAt 8 bs
