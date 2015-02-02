{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main where

import Data.Binary.Put
import qualified Data.ByteString.Lazy as B
import System.Environment
import System.IO

import EscPos
import Serial
import Loader

main :: IO ()
main = do
  [printerDev, imagePath] <- getArgs
  Serial{..} <- openSerialOutRaw printerDev 19200 RtsCts
  -- Enable printer status
  B.hPut handle "\GSa\x24"
  image <- loadMonochrome imagePath
  B.hPut handle $ runPut $ do
    putImage image
    feedPaperOut
  drain
  hClose handle
