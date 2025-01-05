#!/bin/bash -eu

_SCRIPT_DIR=$(
    cd $(dirname $0)
    pwd
)

cmake \
    -S "${_SCRIPT_DIR}" \
    -B "${_SCRIPT_DIR}"/build \
    -G Ninja \
    -DCMAKE_C_COMPILER=clang \
    -DCMAKE_CXX_COMPILER=clang++ \
    -DCMAKE_CXX_FLAGS="-std=c++23 -stdlib=libc++"

cmake --build "${_SCRIPT_DIR}"/build
