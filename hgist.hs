{-# LANGUAGE OverloadedStrings #-}
module HGist where

import System.Environment (getArgs) 
import Control.Applicative ((<$>), (<*>), empty)
import Control.Monad (liftM)
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import Network.HTTP.Conduit
import qualified Data.HashMap.Strict as H
import qualified Data.ByteString.Lazy.Char8 as BL 

data Gist = Gist {description :: String, files :: [File]} deriving (Show, Eq)

data File = File {filename :: String} deriving (Show, Eq)

instance FromJSON Gist where
    parseJSON (Object v) = Gist <$> v .: "description" <*> (v .: "files" >>= parseFiles)
    parseJSON _ = empty

instance FromJSON File where
    parseJSON (Object v) = File <$> v .: "filename"
    parseJSON _ = empty

parseFiles (Object v) = mapM parseJSON (H.elems v)
parseFiles _ = empty

showGist :: Gist -> String
showGist g = "* " ++ description g ++ "\n" ++ showFiles (files g)
    where 
        showFiles files = "    " ++ intercalate ", " (map filename files)

decodeGistList :: BL.ByteString -> [Gist] 
decodeGistList = fromMaybe [] . decode

findGistListForUser :: String -> IO [Gist]
findGistListForUser username =
    liftM decodeGistList $ simpleHttp ("https://api.github.com/users/" ++ username ++ "/gists")
    
main :: IO ()
main = do
    [user]<- getArgs
    findGistListForUser user >>= mapM_ (putStrLn . showGist)
