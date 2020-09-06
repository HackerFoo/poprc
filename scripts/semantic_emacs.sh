#!/usr/bin/env bash

# Use with iterm2:
# Preferences > Advanced > Semantic History > Always run command... > ~/bin/semantic_emacs.sh \5 \1 \2

if [ -z "$1" ]; then
    exit 1
fi

EMACS_PATH=/Applications/Emacs.app/Contents/MacOS/bin

# split file.c:123
ARR=(${2//:/ })

# this is the dir to look for the file
DIR="$1"/$(dirname "${ARR[0]}")

# file basename
FILE=$(basename "${ARR[0]}")

# line number
LINE=${ARR[1]}

# find the first match
PATH=$(find "$DIR" -name "$FILE" -print -quit)

# send it to emacs
if [ -z "${LINE}" ]; then
    $EMACS_PATH/emacsclient -n ${PATH}
else
    $EMACS_PATH/emacsclient -n +${LINE} ${PATH}
fi
