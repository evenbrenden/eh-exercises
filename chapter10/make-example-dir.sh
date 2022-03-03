#! /usr/bin/env bash

count=1000;
dest=example-dir

mkdir $dest

for i in $(seq -w 1 $count)
do
    cp example-text.txt $dest/"$i".txt
done
