# This is the main nix file for termonad.  It will build and wrap the termonad
# binary. It can be built with the command `nix-build` in the main top
# directory.
#
# The termonad executable will be created at `result/bin/termonad`.

{ compiler ? "ghc843" }:

(import <nixpkgs> {}).callPackage
  (import .nix-helpers/termonad-with-packages.nix { inherit compiler; }) {}

