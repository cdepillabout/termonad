# Running `nix-shell .nix-helpers/running-termonad.nix` will put us in an environment
# with termonad available, as well as GHC and a few other packages (lens and colour).
#
# If you run `termonad` while in this environment, `termonad` should be able to see
# GHC and all the Haskell libraries listed below.  This will let `termonad` be able to
# recompile the `~/.config/termonad/termonad.hs` file.
#
# This file is really only used when you want to run termonad in an environment where it
# has access to specific libraries.

{ compiler ? "ghc843" }:

let
  nixpkgs = import ./nixpkgs.nix;

  termonad = nixpkgs.callPackage ../. { inherit compiler; };

  ghcStuff = nixpkgs.pkgs.haskell.packages.${compiler}.ghcWithPackages (pkgs: [
    pkgs.colour
    pkgs.lens
    termonad
  ]);

in

nixpkgs.runCommand "dummy" { buildInputs = [ ghcStuff termonad ]; } ""
