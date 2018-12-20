module Messaging where

import System.IO (hPrint, hPutStrLn, stderr)

errPutStrLn :: String -> IO ()
errPutStrLn = hPutStrLn stderr

verboseMessage :: String -> IO ()
verboseMessage = errPutStrLn

errPrint :: Show a => a -> IO ()
errPrint = hPrint stderr

verbosePrint :: Show a => a -> IO ()
verbosePrint = errPrint
