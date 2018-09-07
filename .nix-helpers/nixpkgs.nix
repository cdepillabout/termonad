# This file pins the version of nixpkgs to a known good version. The nixpkgs is
# imported with an overlay overriding haskellPackages to generate haddocks for
# GI dependencies, and to use the GHC, VTE, GTK and open-haddock versions we
# want. It is imported from various other files.

{ compiler ? "ghc843" }:

let
  nixpkgsTarball = builtins.fetchTarball {
    # recent version of nixpkgs as of 2018-07-29
    url = "https://github.com/NixOS/nixpkgs/archive/a2c6dbe370160ffea5537f64dda04489184c5ce1.tar.gz";
    sha256 = "1x993g9343yv5wyp29i6vskdcc3rl42xipv79nwmmrj8ay2yhh3b";
  };

  set-gi-vte-version = _: {
    version = "2.91.19";
    sha256 = "1hnhidjr7jh7i826lj6kdn264i592sfl5kwvymnpiycmcb37dd4y";
  };

  set-gi-gtk-version = _: {
    version = "3.0.24";
    sha256 = "14cyj1acxs39avciyzqqb1qa5dr4my8rv3mfwv1kv92wa9a5i97v";
  };

  overrideTC = lib: compiler == "ghcHEAD" ||
    lib.strings.toInt (lib.strings.removePrefix "ghc" compiler) > 822;

  haskellPackagesOL = self: super: with super.haskell.lib; {
    haskellPackages = super.haskell.packages.${compiler}.override {
      overrides = hself: hsuper: {
        gi-gdk = doHaddock hsuper.gi-gdk;
        gi-gio = doHaddock hsuper.gi-gio;
        gi-glib = doHaddock hsuper.gi-glib;
        gi-gtk = doHaddock (overrideCabal hsuper.gi-gtk set-gi-gtk-version);
        gi-pango = doHaddock hsuper.gi-pango;
        gi-vte = let
          g3-vte = addPkgconfigDepend
            (hsuper.gi-vte.override { vte = super.gnome3.vte; })
            super.gnome3.gtk;
        in doHaddock (overrideCabal g3-vte set-gi-vte-version);
        termonad = hself.callPackage ./termonad.nix {
          inherit (self.gnome3) gtk3;
        };
        open-haddock = hsuper.open-haddock.overrideAttrs (oa: {
          src = super.fetchFromGitHub {
            owner = "jml";
            repo = "open-haddock";
            rev = "472d10d61d7b9262626171af0484a65365863fa6";
            sha256 = "072d680j1k3n0vkzsbghhnah2p799yxrm7mhvr0nkdvr7iy04gcz";
          };
        });
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

