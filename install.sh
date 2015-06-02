#!/bin/bash

# compile
ghc -o heather Heather.hs

# copy
echo "Copying to /usr/local/bin"
mv -f heather /usr/local/bin
