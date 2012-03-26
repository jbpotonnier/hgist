module Main where

import HGist (listGists, deleteGist, createGist)
import qualified Data.ByteString.Char8 as BS
import System.Environment (getArgs) 
import System.IO (hGetEcho, hSetEcho, hFlush, stdin, stdout)
import Control.Exception (bracket_)

getPassword :: IO String
getPassword = do
  putStr "Password: "
  hFlush stdout
  pass <- withEcho False getLine
  putChar '\n'
  return pass

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

dispatch :: [String] -> IO ()
dispatch ("ls" : user: []) = listGists user
dispatch ("rm" : user : gistId : []) = do 
  password <- getPassword
  deleteGist (BS.pack user) (BS.pack password) gistId
dispatch ("create" : user : description : filenames) = do
  password <- getPassword
  createGist (BS.pack user) (BS.pack password) description filenames
dispatch _ = error "Bad usage"
  
  
main :: IO ()
main = do
    args <- getArgs
    dispatch args

