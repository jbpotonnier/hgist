{-# LANGUAGE OverloadedStrings #-}
module HGist where

import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson
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

decodeGistList :: BL.ByteString -> Maybe [Gist] 
decodeGistList = decode
