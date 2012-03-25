# Hgist
## Description
This software enables to create, delete and list your gists using command line.

## Installation
You will need `http-conduit` and `aeson` to build this program :

    cabal install http-conduit
    cabal install aeson


Then you can compile using :

    ghc -o hgist Main.hs  


## Usage
- `hgist ls <user>` : list gist for user `user`
- `hgist rm <user> <password> <gistId>` : delete gist having `gistId` as id
- `hgist create <user> <password> <description> <file>...` : create a gist containing the files listed and havinf `description` as description. 
