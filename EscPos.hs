-- |Converts monochrome images to Cashino byte array
{-# LANGUAGE OverloadedStrings #-}
module EscPos where

import Data.Array.Unboxed
import Data.Binary.Put
import Data.Binary.Bits.Put (runBitPut, putBool)
import Data.Word
import Common

putImage :: MonochromeImage -> Put
putImage img = do
  putByteString "\GSv00"
  bytes (xMax + 1) >>= putWord16le
  putWord16le $ fromIntegral yMax + 1
  runBitPut $ mapM_ putBool $ elems img
  where ((0,0),(yMax,xMax)) = bounds img
        bytes x = let (q,r) = quotRem x 8
                  in if r == 0
                     then return $ fromIntegral q
                     else fail "Width not multiple of 8"

-- |Feed paper for given number of millimeters.
feedPaperMm :: Word8 -> Put
feedPaperMm n = do
  putByteString "\ESCJ"
  putWord8 $ 8*n

-- |Feed paper out so everything is visible.
feedPaperOut :: Put
feedPaperOut = putByteString "\n\n\n"
