{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit
import HGist (Gist(..), File(..), decodeGistList, showGist)

tests = test [
    "(decodeGistList)" ~:
        [Gist {description = "My description", files = [ File {filename = "myfile.hs"}]}]
        ~=?
        (decodeGistList "[{\"description\": \"My description\", \"files\": {\"myfile.hs\": {\"filename\": \"myfile.hs\"}}}]"),

    "(showGist)" ~:
        (showGist $ Gist {
                    description = "My description", 
                    files = [File {filename = "myFile.hs"}, File {filename = "myOtherFile.hs"}]})
        ~=? "* My description\n    myFile.hs, myOtherFile.hs"
    ]

main = runTestTT tests

