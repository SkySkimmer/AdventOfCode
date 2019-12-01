#!/usr/bin/env bash

m=0

for i in $(cat input.txt); do
    m=$((m + ((i / 3) - 2)))
done

echo $m
