{-# LANGUAGE ForeignFunctionInterface, RecordWildCards #-}
module Serial (Serial(..), openSerialOutRaw) where

import Foreign.C
import System.IO
import System.Posix.IO
import System.Posix.Types

data Serial = Serial { handle :: Handle
                     , drain  :: IO ()
                     , pause  :: IO ()
                     , resume :: IO ()
                     }

foreign import ccall unsafe "init_serial_port" init_serial_port :: CInt -> CInt -> IO CInt
foreign import ccall unsafe "tcdrain" tcdrain :: CInt -> IO CInt
foreign import ccall unsafe "tcflow_off" tcflow_off :: CInt -> IO CInt
foreign import ccall unsafe "tcflow_on" tcflow_on :: CInt -> IO CInt

-- |Opens serial port for output in raw 8-bit mode using given speed
-- and returns functions for closing and writing to the port. It
-- supports even the non-standard bit rates like 250000 bps. In raw
-- 8-bit mode the output is not altered by the operating system.
openSerialOutRaw :: FilePath -> Int -> IO Serial
openSerialOutRaw file speed = do
  Fd fd <- openFd file ReadWrite Nothing OpenFileFlags{ append    = False
                                                      , exclusive = False
                                                      , noctty    = True
                                                      , nonBlock  = False
                                                      , trunc     = False
                                                      }
  throwErrnoIfMinus1Retry_ "Unable to configure serial port" $
    init_serial_port fd (fromIntegral speed)

  handle <- fdToHandle $ Fd fd
  return Serial{ drain = hFlush handle >> drainSerial fd
               , pause = throwErrnoIfMinus1Retry_ "Unable to pause" $ tcflow_off fd
               , resume = throwErrnoIfMinus1Retry_ "Unable to resume" $ tcflow_on fd
               , ..
               }

-- |Drain serial port buffers, including hardware buffer. This
-- supports retrying if interrupted, unlike `drainOutput` in
-- System.Posix.Terminal.
drainSerial :: CInt -> IO ()
drainSerial fd = throwErrnoIfMinus1Retry_ "Unable to drain buffers" $ tcdrain fd
