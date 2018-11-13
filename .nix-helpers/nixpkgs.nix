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
          url = "https://github.com/NixOS/nixpkgs/archive/237285295764fb063ec1ca509c36b17c4990eeb4.tar.gz";
          sha256 = "1cl40yz7ry6x2nbzpc5pkf0q5p0fxvi0c2n7la0pz5g1n80n4xlq";
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

  myfocuslist = callCabal2nix:
    let
      src = builtins.fetchTarball {
        url = "https://github.com/cdepillabout/focuslist/archive/80bd865e82ab4499ccebcd89989d2dbb221bb381.tar.gz";
        sha256 = "1b7da9ngk34jc2w4hhqq6qv20pkch5vvi34kr81xpmr3mmiwqmai";
      };
    in callCabal2nix "focuslist" src {};

  gobjIntroOverride = oldAttrs: {
    patches = oldAttrs.patches ++ [ ./macos-gobject-introspection-rpath.patch ];
  };

  haskellPackagesOL = self: super: with super.haskell.lib; {
    haskellPackages = super.haskell.packages.${compilerVersion}.override {
      overrides = hself: hsuper: {
        # Only override the version of foculist if it doesn't already exist in
        # the haskell package set.
        focuslist = hsuper.focuslist or (myfocuslist hself.callCabal2nix);

        # Set the haskell-gi libraries to generate documentation.
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

    # Darwin needs a patch to gobject-introspection:
    # https://github.com/NixOS/nixpkgs/pull/46310
    gobjectIntrospection = super.gobjectIntrospection.overrideAttrs (oldAttrs: {
      patches =
        oldAttrs.patches ++
        (if self.stdenv.isDarwin
          then [ ./macos-gobject-introspection-rpath.patch ]
          else [ ]);
    });
  };

in import nixpkgsSrc { overlays = [ haskellPackagesOL ]; }

