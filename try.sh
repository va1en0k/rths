#!/bin/bash -ex

# we want to fail compilation before the commit
ghc Main.hs -j -fexcess-precision -optc-O3 -optc-ffast-math -rtsopts -threaded -prof -fprof-auto

git add *.hs || true
git add */*.hs || true
git add */*/*.hs || true
git add 'out/*'

git commit -a -m "${1:-"tryin"}"

HASH=`git rev-parse --short HEAD`

date


echo $HASH >> timelog.txt

{ time ./Main $HASH +RTS -p -N 2> stderr.log || (cat stderr.log && false); } 2> >(tee -a timelog.txt >&2)

echo >> timelog.txt
echo >> timelog.txt

# cat stderr.log
