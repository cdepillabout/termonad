# This is the main nix file for Termonad.  It will build the `termonad`
# executable.  It will place it in a "wrapped" environment so that Termonad can
# find GHC and a few Haskell libraries. This wrapped Termonad will be able to
# rebuild its configuration file (which should be located at
# `~/.config/termonad/termonad.hs`).
#
# Termonad can be built with the command `nix-build` in the main top
# directory.
#
# The `termonad` executable will be created at `result/bin/termonad`.  With
# this default setup, you will be able to use the haskell packages `lens` and
# `colour` in your `~/.config/termonad/termonad.hs` file, as well as the
# Termonad package itself.
#
# If you want to install termonad into your environment, you can use `nix-env`
# from the main top directory:
#
# $ nix-env --file default.nix --install

{ compiler ? null, nixpkgs ? null, additionalOverlays ? [] }@args:

import .nix-helpers/termonad-with-packages.nix args

