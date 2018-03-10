#!/bin/bash

if [ ! -d .cabal-sandbox ]
then
	cabal sandbox init
fi
cabal install --only-dependencies
cabal run
