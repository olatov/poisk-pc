#!/usr/bin/env sh

cd deps

if [ ! -d "pas8088" ]; then
    git clone https://github.com/olatov/pas8088.git
fi

if [ ! -d "ray4laz" ]; then
    git clone https://github.com/GuvaCode/ray4laz.git
    cd ray4laz
    git checkout 5.6.2
    cd ..
fi

cd ..

docker run \
    --rm \
    -v "$(pwd)/src:/src" \
    -v "$(pwd)/deps/:/deps" \
    -v "$(pwd)/out:/out" \
    olatov/fpc:20260115 \
    fpc -O2 -CX -XX -Xs /src/poisk.lpr -Fu/deps/pas8088/src -Fu/deps/ray4laz/source -Fl/deps/ray4laz/libs/x86_64-linux -o/out/poisk 
