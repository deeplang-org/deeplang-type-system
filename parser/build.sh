#!/bin/bash

cd output
rm -r -f *
cd ../
bnfc --ocaml -m deeplang.cf -o output
cd output
make
cd ../