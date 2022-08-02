#!/bin/bash
set -e

: ${1?' no name'}

dir=$(mktemp -d dist-docs.XXXXXX)
trap 'rm -rf "$dir"' EXIT

cabal v2-sdist $1 --builddir="$dir"
ls $dir
cabal upload $2 "$dir/sdist/$(cabal info $1/ | head -n 1 | grep -oP "(?<=\\* )\S+").tar.gz" || true

# assumes cabal 2.4 or later
cabal v2-haddock $1 --builddir="$dir" --haddock-for-hackage --enable-doc

cabal upload $2 -d "$dir/$(cabal info $1/ | head -n 1 | grep -oP "(?<=\\* )\S+")-docs.tar.gz"
