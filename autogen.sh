#!/bin/sh

libtoolize
aclocal
autoheader
autoconf
automake -a --copy
