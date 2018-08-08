#!/bin/bash -ex

git add *.hs
git add out/*

git commit -a -m ${1:-"tryin"}

HASH=`git rev-parse --short HEAD`

ghc Main.hs -j -fexcess-precision -optc-O3 -optc-ffast-math -no-recomp -prof -fprof-auto -rtsopts -threaded

date

time ./Main $HASH +RTS -N
