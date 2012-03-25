{-# LANGUAGE OverloadedStrings #-}
module HGist where

import System.Environment (getArgs) 
import Control.Applicative ((<$>), (<*>), empty)
import Control.Monad (liftM, void)
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import Network.HTTP.Conduit
import Network.HTTP.Types (Method)
import qualified Data.HashMap.Strict as H
import qualified Data.ByteString.Lazy.Char8 as BL 
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T

data Gist = Gist {gistId :: String, 
                  description :: Maybe String, 
                  files :: [File]} 
                  deriving (Show, Eq)

data File = File {filename :: String} deriving (Show, Eq)

instance FromJSON Gist where
    parseJSON (Object v) = Gist <$> 
        v .: "id" <*>
        v .:? "description" <*> 
        (v .: "files" >>= parseFiles)
    parseJSON _ = empty

instance FromJSON File where
    parseJSON (Object v) = File <$> v .: "filename"
    parseJSON _ = empty
    
parseFiles (Object v) = mapM parseJSON (H.elems v)
parseFiles _ = empty

showGist :: Gist -> String
showGist g = gistId g ++ ": " ++ fromMaybe "(No description)" (description g) ++ "\n" ++ showFiles (files g)
    where 
        showFiles files = "    " ++ intercalate ", " (map filename files)

decodeGistList :: BL.ByteString -> [Gist] 
decodeGistList = fromMaybe [] . decode

listGists :: String -> IO()
listGists user = findGistListForUser user >>= mapM_ (putStrLn . showGist)
  where findGistListForUser username = 
          liftM decodeGistList $ simpleHttp (gitHubUrl ["users",  username , "gists"])

deleteGist :: BS.ByteString -> BS.ByteString -> String -> IO ()
deleteGist user pass gistId = do
  request <- parseUrl $ gitHubUrl ["gists", gistId]
  authRequest user pass request {method = "DELETE"}
  return ()

createGist :: BS.ByteString -> BS.ByteString -> String -> [String] -> IO ()
createGist user pass description filenames = do
  request <- parseUrl $ gitHubUrl ["gists"]
  files <- mapM loadFile filenames
  let body = encodeGist description files
  authRequest user pass request {method = "POST", requestBody = RequestBodyLBS body}
  return ()
  where 
    loadFile name = do 
      content <- BL.readFile name
      return (name, content)

authRequest ::  BS.ByteString -> BS.ByteString -> Request IO -> IO ()
authRequest user password request =
    withManager $ void . http (applyBasicAuth user password request)

gitHubUrl :: [String] -> String
gitHubUrl = intercalate "/" . (["https://api.github.com"] ++)

encodeGist :: String -> [(String, BL.ByteString)] -> BL.ByteString
encodeGist description files = 
  encode $ object ["description" .= description, 
                   "public" .= True,
                   "files" .= object [T.pack name .= object ["content" .= content] | (name, content) <- files]]

dispatch :: [String] -> IO ()
dispatch ("ls" : user: []) = listGists user
dispatch ("rm" : user :  password : gistId : []) = deleteGist (BS.pack user) (BS.pack password) gistId
dispatch ("create" : user : password : description : filenames) = createGist (BS.pack user) (BS.pack password) description filenames

main :: IO ()
main = do
    args <- getArgs
    dispatch args

