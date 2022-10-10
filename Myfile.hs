module Myfile where

import System.IO(IOMode(..), openFile, hClose, hGetContents, hSetEncoding, utf8, hPutStr)
import System.Directory(doesFileExist)

tgPass :: FilePath
tgPass = "myfe.txt"

isFile :: IO Bool
isFile = doesFileExist tgPass

fileOut :: IO String 
fileOut = do
  h <- openFile tgPass ReadMode
  hSetEncoding h utf8
  con <- hGetContents h 
  putStr (con++"\n")
  hClose h
  return con

fileIn :: String -> IO ()
fileIn str = do
  h <- openFile tgPass WriteMode
  hSetEncoding h utf8
  hPutStr h str 
  hClose h

