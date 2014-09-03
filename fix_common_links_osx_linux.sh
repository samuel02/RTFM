#!/bin/bash

PWD=$(pwd)

rm $PWD/RTFM-core-compiler/src/Common.ml
rm $PWD/RTFM-cOOre-compiler/src/Common.ml
rm $PWD/RTFM-core-compiler/src/Error.ml
rm $PWD/RTFM-cOOre-compiler/src/Error.ml

ln -s $PWD/RTFM-common/Common.ml $PWD/RTFM-core-compiler/src/Common.ml
ln -s $PWD/RTFM-common/Common.ml $PWD/RTFM-cOOre-compiler/src/Common.ml
ln -s $PWD/RTFM-common/Error.ml $PWD/RTFM-core-compiler/src/Error.ml
ln -s $PWD/RTFM-common/Error.ml $PWD/RTFM-cOOre-compiler/src/Error.ml
