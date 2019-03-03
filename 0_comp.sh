#!/bin/sh

cd ./nnevo

echo "rm *.beam"
rm *.beam

for SRC in *.erl
do
    echo "erlc $SRC"
    erlc $SRC
done

cd ..
