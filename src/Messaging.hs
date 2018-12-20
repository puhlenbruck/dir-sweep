module Messaging where

import System.IO (hPrint, hPutStrLn, stderr)

errPutStrLn :: String -> IO ()
errPutStrLn = hPutStrLn stderr

infoMessage :: String -> IO ()
infoMessage = errPutStrLn

errPrint :: Show a => a -> IO ()
errPrint = hPrint stderr

infoPrint :: Show a => a -> IO ()
infoPrint = errPrint
