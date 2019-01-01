# This file pins the version of nixpkgs to a known good version. The nixpkgs is
# imported with an overlay overriding haskellPackages to generate haddocks for
# GI dependencies, and to use the GHC, VTE, GTK and open-haddock versions we
# want. It is imported from various other files.

{ compiler ? null, nixpkgs ? null }:

let
  nixpkgsSrc =
    if isNull nixpkgs
      then
        builtins.fetchTarball {
          # recent version of nixpkgs master as of 2018-12-23
          url = "https://github.com/NixOS/nixpkgs/archive/c31c0558ddad7161a4025117694197264cda9750.tar.gz";
          sha256 = "09xl8fshyyddcm5nw5fkl6fbjlh5szjcdm43ii6jsvykdr516ghp";
        }
      else
        nixpkgs;

  compilerVersion = if isNull compiler then "ghc863" else compiler;

  # The termonad derivation is generated automatically with `cabal2nix`.
  termonadOverride =
    stdenvLib: gnome3: callCabal2nix: overrideCabal:
      let
        src =
          builtins.filterSource
            (path: type: with stdenvLib;
              ! elem (baseNameOf path) [ ".git" "result" ".stack-work" ".nix-helpers" ] &&
              ! any (flip hasPrefix (baseNameOf path)) [ "dist" ".ghc" ]
            )
            ./..;
        termonad = callCabal2nix "termonad" src {
          inherit (gnome3) gtk3;
          vte_291 = gnome3.vte;
        };
      in
      overrideCabal termonad (oldAttrs: {
        # For some reason the doctests fail when running with nix.
        # https://github.com/cdepillabout/termonad/issues/15
        doCheck = false;
      });

  haskellPackagesOverlay = self: super: with super.haskell.lib; {
    haskellPackages = super.haskell.packages.${compilerVersion}.override {
      overrides = hself: hsuper: {
        termonad =
          termonadOverride
            self.stdenv.lib
            self.gnome3
            hself.callCabal2nix
            self.haskell.lib.overrideCabal;

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

    # # Darwin needs a patch to gobject-introspection:
    # # https://github.com/NixOS/nixpkgs/pull/46310
    # gobjectIntrospection = super.gobjectIntrospection.overrideAttrs (oldAttrs: {
    #   patches =
    #     oldAttrs.patches ++
    #     (if self.stdenv.isDarwin
    #       then [ ./macos-gobject-introspection-rpath.patch ]
    #       else [ ]);
    # });
  };

in import nixpkgsSrc { overlays = [ haskellPackagesOverlay ]; }
