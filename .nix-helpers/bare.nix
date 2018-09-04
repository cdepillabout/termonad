# Bare termonad.
#
# This file will build just the termonad executable, but will not wrap it in an
# environment so that it can find GHC.  This means that termonad will not be able
# to recompile its `~/.config/termonad/termonad.hs` file.
#
# You probably should not call `nix-build` on this file directly.  Instead, it
# is a better idea to use the `./termonad-with-packages.nix` file, or just the
# `../default.nix` file.  See the comments in those files for how they work.

{ compiler ? "ghc843" }:

let nixpkgs = import ./nixpkgs.nix { inherit compiler; }; in

nixpkgs.haskellPackages.callPackage ./termonad.nix {
  inherit (nixpkgs.gnome3) gtk3;
}
