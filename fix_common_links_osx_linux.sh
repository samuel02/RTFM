#!/bin/bash

rm ./RTFM-core-compiler/src/Common.ml
rm ./RTFM-cOOre-compiler/src/Common.ml
rm ./RTFM-core-compiler/src/Error.ml
rm ./RTFM-cOOre-compiler/src/Error.ml

ln -s ./RTFM-common/Common.ml ./RTFM-core-compiler/src/Common.ml
ln -s ./RTFM-common/Common.ml ./RTFM-cOOre-compiler/src/Common.ml
ln -s ./RTFM-common/Error.ml ./RTFM-core-compiler/src/Error.ml
ln -s ./RTFM-common/Error.ml ./RTFM-cOOre-compiler/src/Error.ml
