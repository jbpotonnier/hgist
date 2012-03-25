module Main where

import HGist (listGists, deleteGist, createGist)
import qualified Data.ByteString.Char8 as BS
import System.Environment (getArgs) 

dispatch :: [String] -> IO ()
dispatch ("ls" : user: []) = listGists user
dispatch ("rm" : user :  password : gistId : []) = deleteGist (BS.pack user) (BS.pack password) gistId
dispatch ("create" : user : password : description : filenames) = createGist (BS.pack user) (BS.pack password) description filenames
dispatch _ = error "Bad usage"
  
  
main :: IO ()
main = do
    args <- getArgs
    dispatch args

