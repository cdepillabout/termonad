
# This file takes some optional arguments and produces a wrapper around
# termonad that will know where to find a GHC with the libraries needed to
# recompile its config file. This is not NixOS only; it should work with nix
# on any system.
#
# Example usage in an overlay:
#
# > # This file at ~/.config/nixpkgs/overlays/termonad-overlay.nix
# > self: super: {
# >   termonad = import /path/to/termonad-with-packages.nix {
# >     packages = hp: [ hp.colour hp.lens hp.MonadRandom ];
# >   };
# > }
#
# Then termonad can be installed through nix's standard methods, e.g. nix-env.
# If it's to be installed on NixOS through configuration.nix, then the overlay
# too will need to be explicitly declared there, e.g.
# > nixpkgs.overlays = [ (import /path/to/termonad-overlay.nix) ];
#
# Note that packages has a default value; you don't need to define your own.
# To use the default, just pass in {} rather than { packages = ... ; }.

{ packages ? (self: [ self.colour self.lens ])
, compiler ? "ghc843" }:

let
  nixpkgs  = import ./nixpkgs.nix;
  ghcWithPackages = nixpkgs.haskell.packages."${compiler}".ghcWithPackages;
  termonad = import ./bare.nix { inherit compiler; };
  env = ghcWithPackages (self: [ termonad ] ++ packages self);
in

nixpkgs.stdenv.mkDerivation {
  name = "termonad-with-packages-${env.version}";
  nativeBuildInputs = [ nixpkgs.makeWrapper ];
  buildCommand = ''
    mkdir -p $out/bin
    makeWrapper ${env}/bin/termonad $out/bin/termonad \
      --set NIX_GHC "${env}/bin/ghc"
  '';
  preferLocalBuild = true;
  allowSubstitutes = false;
}
