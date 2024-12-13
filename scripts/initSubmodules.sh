#!/bin/sh

# exit when any command fails
set -e

CURRENT=$(pwd);

git submodule init
git submodule update

cd $CURRENT/ts-parser/haskell-tree-sitter
git submodule init
git submodule update -- ./tree-sitter/vendor/tree-sitter

git submodule init
git submodule update -- ./tree-sitter-haskell/vendor/tree-sitter-haskell
