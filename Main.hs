{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main where

import Data.Binary.Put
import qualified Data.ByteString.Lazy as B
import System.Environment
import System.IO

import EscPos
import Serial
import Loader

main = do
  [printerDev, imagePath] <- getArgs
  Serial{..} <- openSerialOutRaw printerDev 19200
  image <- loadMonochrome imagePath
  B.hPut handle $ runPut $ putImage image
  B.hPut handle "\n\n\n"
  drain
  hClose handle
