#!/usr/bin/env bash
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
trap 'rm -rf "$dir"' EXIT

cabal new-haddock --builddir="$dir" --haddock-for-hackage --haddock-hyperlink-source # --haddock-quickjump # quickjump option doesn't work???
cabal upload --publish --documentation $dir/*-docs.tar.gz
