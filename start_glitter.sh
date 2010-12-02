#!/bin/sh

VERSION=$(cat VERSION | tr -d '\n')
PWD=$(dirname $0)
CONFIG=$1

make

if [[ ! -f ebin/glitter*.boot ]]; then
	make boot
fi

erl -pa $PWD/ebin \
    -glitter config_file "\"$CONFIG\"" \
    -boot glitter-$VERSION
