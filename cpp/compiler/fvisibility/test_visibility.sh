#!/usr/bin/bash

# https://stackoverflow.com/questions/10749058/building-and-linking-a-shared-library

g++ -Wall -fpic fvisibility.cc -c -o libfvis.so
g++ -Wall main.cc -c -o main.o
g++ -Wall -fvisibility=hidden main.o -I. -L. -lfvis

rm main.o
