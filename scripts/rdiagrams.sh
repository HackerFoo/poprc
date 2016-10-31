#!/bin/bash

# for viewing diagrams.html on a remote machine (e.g. phone)

ssh -L7072:localhost:7072 "$1" "cd ~/src/poprc && python3 -m http.server 7072"
