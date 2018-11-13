# This file pins the version of nixpkgs to a known good version. The nixpkgs is
# imported with an overlay overriding haskellPackages to generate haddocks for
# GI dependencies, and to use the GHC, VTE, GTK and open-haddock versions we
# want. It is imported from various other files.

{ compiler ? null, nixpkgs ? null }:

let
  # recent version of nixpkgs as of 2018-11-09
  nixpkgsSrc =
    if isNull nixpkgs
      then
        builtins.fetchTarball {
          url = "https://github.com/NixOS/nixpkgs/archive/1c49226176d248129c795f4a654cfa9d434889ae.tar.gz";
          sha256 = "0v8vp38lh6hqazfwxhngr5j96m4cmnk1006kh5shx0ifsphdip62";
        }
      else
        nixpkgs;

  compilerVersion = if isNull compiler then "ghc844" else compiler;

  # The termonad derivation is generated automatically with `cabal2nix`.
  termonadOverride =
    stdenvLib: gnome3: callCabal2nix: overrideCabal:
      let
        src =
          builtins.filterSource
            (path: type: with stdenvLib;
              ! elem (baseNameOf path) [ ".git" "result" ".stack-work" ] &&
              ! any (flip hasPrefix (baseNameOf path)) [ "dist" ".ghc" ]
            )
            ./..;
        termonad = callCabal2nix "termonad" src {
          inherit (gnome3) gtk3;
        };
      in
      overrideCabal termonad (oldAttrs: {
        # For some reason the doctests fail when running with nix.
        # https://github.com/cdepillabout/termonad/issues/15
        doCheck = false;
      });

  haskellPackagesOL = self: super: with super.haskell.lib; {
    haskellPackages = super.haskell.packages.${compilerVersion}.override {
      overrides = hself: hsuper: {
        gi-gdk = doHaddock hsuper.gi-gdk;
        gi-gio = doHaddock hsuper.gi-gio;
        gi-glib = doHaddock hsuper.gi-glib;
        gi-gtk = doHaddock hsuper.gi-gtk;
        gi-pango = doHaddock hsuper.gi-pango;
        gi-vte = doHaddock hsuper.gi-vte;
        termonad = termonadOverride self.stdenv.lib self.gnome3 hself.callCabal2nix self.haskell.lib.overrideCabal;
        # This is a tool to use to easily open haddocks in the browser.
        open-haddock = hsuper.open-haddock.overrideAttrs (oa: {
          src = super.fetchFromGitHub {
            owner = "jml";
            repo = "open-haddock";
            rev = "472d10d61d7b9262626171af0484a65365863fa6";
            sha256 = "072d680j1k3n0vkzsbghhnah2p799yxrm7mhvr0nkdvr7iy04gcz";
          };
        });
      };
    };
  };

in import nixpkgsSrc { overlays = [ haskellPackagesOL ]; }

