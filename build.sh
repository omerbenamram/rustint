#!/usr/bin/bash -eu

_SCRIPT_DIR=$(cd $(dirname $0); pwd)

cmake \
    -S "${_SCRIPT_DIR}" \
    -B "${_SCRIPT_DIR}"/build \
    -G Ninja \
    -DCMAKE_C_COMPILER=clang-20 \
    -DCMAKE_CXX_COMPILER=clang++-20 \
    #

cmake --build "${_SCRIPT_DIR}"/build
