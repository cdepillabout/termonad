{ compiler ? "ghc843" }:

let
  config = {
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          gi-vte = haskellPackagesOld.gi-vte.override { vte = pkgs.gnome3.vte; };
        };
      };
    };
  };

  nixpkgsTarball = builtins.fetchTarball {
    # recent version of nixpkgs as of 2018-07-29
    url = "https://github.com/NixOS/nixpkgs/archive/a2c6dbe370160ffea5537f64dda04489184c5ce1.tar.gz";
    sha256 = "1x993g9343yv5wyp29i6vskdcc3rl42xipv79nwmmrj8ay2yhh3b";
  };
  nixpkgs = import nixpkgsTarball { inherit config; };
in

nixpkgs.pkgs.haskell.packages.${compiler}.callPackage .nix-helpers/termonad.nix { }

