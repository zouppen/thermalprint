{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main where

import Control.Monad (when)
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
  s@Serial{..} <- openSerialOutRaw printerDev 19200
  image <- loadMonochrome imagePath
  putXonXoff s $ runPut $ putImage image
  B.hPut handle "\n\n\n"
  drain
  hClose handle

-- |Some or even all USB serial devices have no support for XON/XOFF
-- and they have so large output buffers which make proper XON/XOFF
-- impossible even though Linux supports it. This function implements
-- quick and dirty software handshake.
putXonXoff :: Serial -> B.ByteString -> IO ()
putXonXoff s@Serial{..} bs = do
  B.hPut handle now
  drain
  status <- B.hGetNonBlocking handle 1
  when (status == "\DC3") $ do
    putStrLn "STOPPED"
    _ <- B.hGet handle 1
    putStrLn "RESUMED"
  if B.null later
    then return ()
    else putXonXoff s later
  where (now, later) = B.splitAt 128 bs
