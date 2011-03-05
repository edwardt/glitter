#!/bin/sh

VERSION=$(cat VERSION | tr -d '\n')
PWD=$(dirname $0)
CONFIG=$1

make

if [[ ! -f $PWD/ebin/glitter*.boot ]]   
  then
	make boot local
  fi

exec erl -pa \ 
    $PWD/ebin \
    $PWD/deps/*ebin \
    -boot $PWD/glitter-$VERSION \
    -s reloader \
    -glitter config_file "\"$CONFIG\""

#erl -pa $PWD/ebin \
#    -glitter config_file "\"$CONFIG\"" \
#    -boot glitter-$VERSION
