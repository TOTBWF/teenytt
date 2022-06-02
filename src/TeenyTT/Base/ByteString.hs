-- |
module TeenyTT.Base.ByteString
  ( module BS
  , sliceLine
  ) where

import Prelude hiding (takeWhile)

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.UTF8 qualified as UTF8_BS

takeWhile :: (Char -> Bool) -> ByteString -> ByteString
takeWhile p bs = loop 0 bs
    where
      loop nbytes bs1 =
          case UTF8_BS.decode bs1 of
            Just (c, n) | p c -> loop (nbytes + n) (BS.drop n bs1)
            _ -> BS.take nbytes bs


sliceLine :: Int -> ByteString -> ByteString
sliceLine start bs = takeWhile (/= '\n') $ UTF8_BS.drop start bs
