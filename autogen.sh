#!/bin/sh

generate_changelog() {
    echo ">>> Generating ChangeLog file" 1>&2

    if git rev-parse --git-dir 2>&1 > /dev/null; then
        git log > ChangeLog
    fi
}

run() {
    echo ">>> $@" 1>&2

    if ! $@ ; then
        echo "Error!" 1>&2
        exit 1
    fi
}

configure=false

while getopts cd: OPT ; do
    case "$OPT" in
        c)
            configure=true
            ;;
    esac
done

generate_changelog
run mkdir -p config
run libtoolize --copy --force --automake
run aclocal
run autoheader
run autoconf
run automake -a --copy

if $configure ; then
    ./configure
fi
