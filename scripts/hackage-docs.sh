#!/bin/bash
#
# This script should be run to generate and upload documentation for Hackage.
# First, you should get into a nix-shell that has cabal-install available:
#
# $ cd termonad/
# $ nix-shell
#
# Then you can just run this script:
#
# $ ./scripts/hackage-docs.sh
#
# This should create and upload documentation for Termonad to Hackage.

set -e

dir=$(mktemp -d dist-docs.XXXXXX)
trap 'rm -r "$dir"' EXIT

cabal configure --builddir="$dir"
cabal haddock --builddir="$dir" --for-hackage --haddock-option=--hyperlinked-source
cabal upload  --publish -d $dir/*-docs.tar.gz
