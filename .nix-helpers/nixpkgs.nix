# This file pins the version of nixpkgs to a known good version. The nixpkgs is
# imported with an overlay overriding haskellPackages to generate haddocks for
# GI dependencies, and to use the GHC, VTE, GTK and open-haddock versions we
# want. It is imported from various other files.

{ compiler ? "ghc843" }:

let
  # recent version of nixpkgs as of 2018-10-17
  nixpkgsTarball = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/6a23e11e658b7a7a77f1b61d30d64153b46bc852.tar.gz";
    sha256 = "03n4bacfk1bbx3v0cx8xcgcmz44l0knswzh7hwih9nx0hj3x41yc";
  };

  # The ./termonad.nix derivation has been generated with `cabal2nix`.
  # It can be updated with the following command:
  #
  # ```sh
  # $ cabal2nix .. > termonad.nix
  # ```
  #
  # Below, we override some default values in the ./termonad.nix derivation.  This
  # allows us to directly use the ./termonad.nix generated with cabal2nix
  # without having to manually change things in that file.
  termonadOverride = stdenvLib: gnome3: haskellCallPackage: overrideCabal:
    let
      termonad = haskellCallPackage ./termonad.nix {
        inherit (gnome3) gtk3;
      };
    in
    overrideCabal termonad (oldAttrs: {
      # For some reason the doctests fail when running with nix.
      # https://github.com/cdepillabout/termonad/issues/15
      doCheck = false;
      # Filter some unnecessary files from the src.
      src =
        builtins.filterSource
          (path: type: with stdenvLib;
            ! elem (baseNameOf path) [ ".git" "result" ".stack-work" ] &&
            ! any (flip hasPrefix (baseNameOf path)) [ "dist" ".ghc" ]
          )
          ./..;
    });

  overrideTC = lib: compiler == "ghcHEAD" ||
    lib.strings.toInt (lib.strings.removePrefix "ghc" compiler) > 822;

  haskellPackagesOL = self: super: with super.haskell.lib; {
    haskellPackages = super.haskell.packages.${compiler}.override {
      overrides = hself: hsuper: {
        gi-gdk = doHaddock hsuper.gi-gdk;
        gi-gio = doHaddock hsuper.gi-gio;
        gi-glib = doHaddock hsuper.gi-glib;
        gi-gtk = doHaddock hsuper.gi-gtk;
        gi-pango = doHaddock hsuper.gi-pango;
        gi-vte = doHaddock hsuper.gi-vte;
        termonad = termonadOverride self.stdenv.lib self.gnome3 hself.callPackage self.haskell.lib.overrideCabal;
        # This is a tool to use to easily open haddocks in the browser.
        open-haddock = hsuper.open-haddock.overrideAttrs (oa: {
          src = super.fetchFromGitHub {
            owner = "jml";
            repo = "open-haddock";
            rev = "472d10d61d7b9262626171af0484a65365863fa6";
            sha256 = "072d680j1k3n0vkzsbghhnah2p799yxrm7mhvr0nkdvr7iy04gcz";
          };
        });
        # This is for https://github.com/cdepillabout/termonad/pull/36.
        type-combinators =
          if ! overrideTC super.stdenv.lib then hsuper.type-combinators else
          hsuper.type-combinators.overrideAttrs (oa: {
            src = super.fetchFromGitHub {
              owner = "kylcarte";
              repo = "type-combinators";
              rev = "071942730b6bb34990d37e1a25236382cf0dcbc6";
              sha256 = "09criipy1ry2ala8bmr5np14cdr6ncph8zd0ald152jcrfh0fphm";
            };
          });
      };
    };
  };

in import nixpkgsTarball { overlays = [ haskellPackagesOL ]; }

