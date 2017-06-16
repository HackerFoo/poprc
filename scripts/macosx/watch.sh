#!/bin/sh

SRC="$@"

pushd ${POPRC_ROOT} > /dev/null

update() {
    while read ev; do
        ./eval -load ${SRC} -bc > build/bytecode.out
    done
}

# https://github.com/emcrisostomo/fswatch
fswatch -o ${SRC} | update

popd > /dev/null
