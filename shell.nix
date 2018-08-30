# This is a file that allows you to jump into an environment to be able to build termonad.
# You can jump into this environment by running the command `nix-shell`.
#
# This also installs cabal, so you should be able to build termonad by running `cabal new-build`.
#
# In general, if you prefer to use `stack`, you probably won't use this file.

{ compiler ? "ghc843" }:

let
  nixpkgs = import .nix-helpers/nixpkgs.nix;
in

(import .nix-helpers/bare.nix { inherit compiler; }).env.overrideAttrs (oldAttrs: rec {
  nativeBuildInputs = oldAttrs.nativeBuildInputs ++ [ nixpkgs.pkgs.haskell.packages.${compiler}.cabal-install ];
})
