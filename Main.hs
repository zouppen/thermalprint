{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Binary.Put
import System.Environment
import qualified Data.ByteString.Lazy as B -- TMP

import EscPos
import Serial
import Loader

main = do
  [printerDev, imagePath] <- getArgs
  (close, write) <- openSerialOutRaw printerDev 19200
  image <- loadMonochrome imagePath
  write $ runPut $ putImage image
  write "\n\n\n"
  return ()
