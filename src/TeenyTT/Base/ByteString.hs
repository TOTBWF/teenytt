-- |
module TeenyTT.Base.ByteString
  ( module BS
  , slice
  ) where

import Data.ByteString as BS
import Data.ByteString.Internal

-- | @slice start end bs@ will extract the slice @[start, end]@ out of the bytestring.
-- @
--     slice 1 2 "asdf"
-- @
--
-- > "sd"
slice :: Int -> Int -> ByteString -> ByteString
slice start end bs@(BS ptr len)
    | start > end = empty
    | start >= len = empty
    | end >= len = BS (plusForeignPtr ptr start) (len - start)
    | otherwise = BS (plusForeignPtr ptr start) (end - start + 1)
