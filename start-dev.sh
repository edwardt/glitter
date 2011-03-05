#!/bin/sh

VERSION=$(cat VERSION | tr -d '\n')
PWD=$(dirname $0)
CONFIG=$1
echo "Using current dir: $PWD and Config file: $CONFIG"

make

if [[ ! -f ebin/glitter*.boot ]]; then
        echo "Boot file not found. Generate one"
	make boot
fi

exec erl -pa \ 
    $PWD/ebin \
    $PWD/deps/*ebin \
    -boot glitter-$VERSION \
    -s reloader \
    -glitter config_file "\"$CONFIG\""