#!/usr/bin/env bash
#
# This script should be run to generate and upload documentation for Hackage.
#
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

# Change to the top-level Termonad directory.
this_script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd "$this_script_dir/"
cd ..

docs_dir=$(mktemp -d dist-docs.XXXXXX)
trap 'rm -rf "$docs_dir"' EXIT

cabal new-haddock --builddir="$docs_dir" --haddock-for-hackage --haddock-hyperlink-source # --haddock-quickjump # quickjump option doesn't work???
cabal upload --publish --documentation $docs_dir/*-docs.tar.gz
