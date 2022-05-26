#!/bin/bash

cd output
make || exit
for FILE in ../test/*; do ./TestDeeplang $FILE; done
