#!/bin/bash

# compile
ghc -o heather main.hs

# move
echo "Moving heather to /usr/local/bin"
mv -f heather /usr/local/bin
