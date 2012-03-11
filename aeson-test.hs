{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit

import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson
import Data.ByteString.Lazy.Char8() 
import qualified Data.HashMap.Strict as H

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


tests = test [
    "(decode gist)" ~:
        Just Gist {description = "My description", files = [ File {filename = "myfile.hs"}]}
        ~=?
        (decode "{\"description\": \"My description\", \"files\": {\"myfile.hs\": {\"filename\": \"myfile.hs\"} } }" :: Maybe Gist)
    ]

main = runTestTT tests

