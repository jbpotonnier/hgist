{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit
import HGist (Gist(..), File(..), decodeGistList, showGist, encodeGist)
import qualified Data.ByteString.Lazy.Char8 as BL 

tests = test [
    "(decodeGistList)" ~:
    [Gist {gistId = "1",
           description = Just "My description", 
           files = [ File {filename = "myfile.hs"}]}]
    ~=?
    decodeGistList "[{\"id\": \"1\", \"description\": \"My description\", \"files\": {\"myfile.hs\": {\"filename\": \"myfile.hs\"}}}]",

    "(decodeGistList No description)" ~:
    [Gist {gistId = "2",
           description = Nothing,
           files = [ File {filename = "myfile.hs"}]}]
    ~=?
    decodeGistList "[{\"id\": \"2\", \"description\": null , \"files\": {\"myfile.hs\": {\"filename\": \"myfile.hs\"}}}]",

    "(showGist)" ~:
    "3: My description\n    myFile.hs, myOtherFile.hs"
    ~=?
    showGist Gist {
      gistId = "3",
      description = Just "My description", 
      files = [File {filename = "myFile.hs"}, File {filename = "myOtherFile.hs"}]},
    
    "(encodeGist)" ~:
    "{\"description\":\"my description\",\"public\":true}" 
    ~=?
    (BL.unpack $ encodeGist "my description" [])
    ]
        
main = runTestTT tests
