# This is the main nix file for termonad.  It will just build the termonad binary.
# It can be built with the command `nix-build` in the main top directory.
#
# The termonad binary will be created at `result/bin/termonad`.

{ compiler ? "ghc843" }:

let
  nixpkgs = import ./.nix-helpers/nixpkgs.nix;

  set-gi-vte-version = _: {
    version = "2.91.19";
    sha256 = "1hnhidjr7jh7i826lj6kdn264i592sfl5kwvymnpiycmcb37dd4y";
  };

  set-gi-gtk-version = _: {
    version = "3.0.24";
    sha256 = "14cyj1acxs39avciyzqqb1qa5dr4my8rv3mfwv1kv92wa9a5i97v";
  };

  allHaskellPackages = nixpkgs.pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: let lib = nixpkgs.pkgs.haskell.lib; in {
      gi-gtk = lib.overrideCabal super.gi-gtk set-gi-gtk-version;
      gi-vte = lib.overrideCabal (lib.addPkgconfigDepend (super.gi-vte.override { vte = nixpkgs.gnome3.vte; }) nixpkgs.gnome3.gtk) set-gi-vte-version;
    };
  };

in

allHaskellPackages.callPackage .nix-helpers/termonad.nix {
  inherit (nixpkgs.gnome3) gtk3;
}
