{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit
import HGist

tests = test [
    "(decodeGistList)" ~:
        Just [Gist {description = "My description", files = [ File {filename = "myfile.hs"}]}]
        ~=?
        (decodeGistList "[{\"description\": \"My description\", \"files\": {\"myfile.hs\": {\"filename\": \"myfile.hs\"}}}]") 

    ]

main = runTestTT tests

