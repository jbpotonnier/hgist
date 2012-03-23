{-# LANGUAGE OverloadedStrings #-}
module HGist where

import System.Environment (getArgs) 
import Control.Applicative ((<$>), (<*>), empty)
import Control.Monad (liftM)
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import Network.HTTP.Conduit
import Network.HTTP.Types (Method)
import qualified Data.HashMap.Strict as H
import qualified Data.ByteString.Lazy.Char8 as BL 
import qualified Data.ByteString.Char8 as BS

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

findGistListForUser :: String -> IO [Gist]
findGistListForUser username =
    liftM decodeGistList $ simpleHttp ("https://api.github.com/users/" ++ username ++ "/gists")

listGists :: String -> IO()
listGists user = findGistListForUser user >>= mapM_ (putStrLn . showGist)

deleteGist :: BS.ByteString -> BS.ByteString -> String -> IO ()
deleteGist user pass gistId = authRequest "DELETE" user pass ("https://api.github.com/gists/" ++ gistId)

authRequest :: Method -> BS.ByteString -> BS.ByteString -> String -> IO ()
authRequest httpMethod user password url = do
    request <- parseUrl url
    let postRequest = applyBasicAuth user password $ request { method = httpMethod }
    withManager $ \manager -> do
        response <- http postRequest manager
        return ()


encodeGist :: String -> [(String, BL.ByteString)] -> BL.ByteString
encodeGist description files = encode $ object ["description" .= description, 
                                                "public" .= True,
                                                "files" .= object [ filename .= content | (filename, content) <- files]]

dispatch :: [String] -> IO ()
dispatch ["ls", user] = listGists user
dispatch ["rm", user, password, gistId] = deleteGist (BS.pack user) (BS.pack password) gistId

main :: IO ()
main = do
    args <- getArgs
    dispatch args

