# This file pins the version of nixpkgs to a known good version. The nixpkgs is
# imported with an overlay adding Termonad. It is imported from various other
# files.

{ # String representing a GHC version to use.  Normally something like
  # "ghc865".  If null, then use a known-working GHC version.
  compiler ? null
, # A path to nixpkgs.  This will be imported.  If null, use a known-working
  # nixpkgs version.
  nixpkgs ? null
, # Additional overlays to apply when importing nixpkgs.
  additionalOverlays ? []
, # Build all the examples bundled with termonad.  Normally this is only used
  # in CI for testing that the examples all still compile.
  buildExamples ? false
, # This is only used for `termonadShell`.
  #
  # If this is `true`, Hoogle will also index the Termonad libraries,
  # however this will mean the environment will need to be rebuilt every
  # time the termonad source changes.
  indexTermonad ? false
  # Extra Haskell packages that will be visible by Termonad when it compiles
  # itself.  See ./termonad-with-packages.nix for an example of how to use
  # this.
, extraHaskellPackages ? null
}:

let
  flake-lock = builtins.fromJSON (builtins.readFile ../flake.lock);

  nixpkgsSrc =
    if isNull nixpkgs
      then
        builtins.fetchTarball {
          url = "https://github.com/NixOS/nixpkgs/archive/${flake-lock.nodes.nixpkgs.locked.rev}.tar.gz";
          sha256 = flake-lock.nodes.nixpkgs.locked.narHash;
        }
      else nixpkgs;

  haskellPackagesOverlays = import ./overlays.nix;

  # This overlays sets some of the options use we at development time.
  termonadOptionsOverlay = self: super: {
    termonadCompilerVersion =
      if isNull compiler then super.termonadCompilerVersion else compiler;

    termonadBuildExamples = buildExamples;

    termonadIndexTermonad = indexTermonad;

    termonadExtraHaskellPackages =
      if isNull extraHaskellPackages then super.termonadExtraHaskellPackages else extraHaskellPackages;
  };

in

import nixpkgsSrc {
  overlays =
    haskellPackagesOverlays ++ [ termonadOptionsOverlay ] ++ additionalOverlays;
}
