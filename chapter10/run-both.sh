#! /usr/bin/env bash

# First, do: bash make-example-dir.sh && make all

./metrics-strictness1 +RTS -s -RTS ./example-dir/
./metrics-strictness2 +RTS -s -RTS ./example-dir/
