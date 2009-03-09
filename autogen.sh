#!/bin/sh

run() {
    echo ">>> $@" 1>&2

    if ! $@ ; then
        echo "Error!" 1>&2
        exit 1
    fi
}

run libtoolize --copy --force --automake
run aclocal
run autoheader
run autoconf
run automake -a --copy
