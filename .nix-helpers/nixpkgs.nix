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
          # recent version of nixpkgs master as of 2019-01-09
          url = "https://github.com/NixOS/nixpkgs/archive/beaf69cee298e092698dd2da2e4758b7811859ad.tar.gz";
          sha256 = "0863a8bgb9z2cbjcwp2xqspsbqcnq035k7rfylicxa75gsj9xgk1";
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

        # https://github.com/NixOS/nixpkgs/pull/53682
        genvalidity-hspec = dontCheck hsuper.genvalidity-hspec;
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
