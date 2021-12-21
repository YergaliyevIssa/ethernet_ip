#!/bin/sh
# based on build_deps.sh from basho/eleveldb

set -e

# the script folder
DIR=$PWD
BASEDIR="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"

# dive into c_src
cd $BASEDIR

# detecting gmake and if exists use it
# if not use make
# (code from github.com/tuncer/re2/c_src/build_deps.sh
which gmake 1>/dev/null 2>/dev/null && MAKE=gmake
MAKE=${MAKE:-make}

# This is a workaround of compilation error of cJSON:
#   generic selections are a C11-specific feature
TARGET_OS=`uname -s`
if [ "$TARGET_OS" = "Darwin" ] || [ "$TARGET_OS" = "FreeBSD" ]; then
    export CFLAGS="$CFLAGS -Wno-c11-extensions"
fi

case "$1" in
    clean)
        rm -rf cJSON
        rm -rf libplctag
        ;;

    build)

        # cJSON
        cd $BASEDIR
        if [ ! -d cJSON ]; then
            git clone --depth 1 -b v1.7.14 https://github.com/DaveGamble/cJSON.git
        fi
        cd cJSON
        mkdir -p build
        cd build
        cmake cmake .. -DENABLE_CJSON_UTILS=On -DENABLE_CJSON_TEST=Off -DCMAKE_INSTALL_PREFIX=_install -DBUILD_SHARED_LIBS=Off
        make && make install

        #libplctag
        cd $BASEDIR
        if [ ! -d libplctag ]; then
            git clone https://github.com/libplctag/libplctag.git
        fi
        cd libplctag
        mkdir -p build
        cd build
        cmake cmake .. -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=_install -DBUILD_SHARED_LIBS=Off
        make && make install

        ;;
esac

cd $DIR
