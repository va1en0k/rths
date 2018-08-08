#!/bin/bash -ex

# we want to fail compilation before the commit
ghc Main.hs -j -fexcess-precision -optc-O3 -optc-ffast-math -prof -fprof-auto -rtsopts -threaded

git add *.hs
git add out/*

git commit -a -m "${1:-"tryin"}"

HASH=`git rev-parse --short HEAD`

date

time ./Main $HASH +RTS -N
