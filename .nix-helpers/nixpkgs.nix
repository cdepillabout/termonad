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
}:

let
  nixpkgsSrc =
    if isNull nixpkgs
      then
        builtins.fetchTarball {
          # Recent version of nixpkgs master as of 2020-08-18 which uses LTS-16.9.
          url = "https://github.com/NixOS/nixpkgs/archive/c5815280e92112a25d958a2ec8b3704d7d90c506.tar.gz";
          sha256 = "09ic4s9s7w3lm0gmcxszm5j20cfv4n5lfvhdvgi7jzdbbbdps1nh";
        }
      else nixpkgs;

  compilerVersion = if isNull compiler then "ghc884" else compiler;

  # An overlay that adds termonad to all haskell package sets.
  haskellPackagesOverlay = self: super: {
    haskell = super.haskell // {
      packageOverrides = hself: hsuper:
        super.haskell.packageOverrides hself hsuper // {
          termonad =
            let
              filesToIgnore = [
                ".git"
                ".nix-helpers"
                "result"
                ".stack-work"
                ".travis.yml"
              ];

              src =
                builtins.filterSource
                  (path: type: with self.stdenv.lib;
                    ! elem (baseNameOf path) filesToIgnore &&
                    ! any (flip hasPrefix (baseNameOf path)) [ "dist" ".ghc" ]
                  )
                  ./..;

              extraCabal2nixOptions =
                self.lib.optionalString buildExamples "-fbuildexamples";

              termonadDrv =
                hself.callCabal2nixWithOptions
                  "termonad"
                  src
                  extraCabal2nixOptions
                  {
                    inherit (self) gtk3;
                    vte_291 = self.vte;
                  };
            in
            termonadDrv;
        };
    };

    # A Haskell package set where we know the GHC version works to compile
    # Termonad.  This is basically just a shortcut so that other Nix files
    # don't need to figure out the correct compiler version to use when it is
    # not given by the user.
    termonadKnownWorkingHaskellPkgSet = self.haskell.packages.${compilerVersion};

    # This is a shell environment for hacking on Termonad with cabal.  See the
    # top-level shell.nix for an explanation.
    termonadShell =
      let
        # Nix-shell environment for hacking on termonad.
        termonadEnv = self.termonadKnownWorkingHaskellPkgSet.termonad.env;

        # Build tools that are nice to have.  It is okay to get Haskell build tools
        # from any Haskell package set, since they do not depend on the GHC version
        # we are using.  We get these from the normal haskellPackages pkg set because
        # then they don't have to be compiled from scratch.
        convenientNativeBuildTools = [
          self.cabal-install
          self.gnome3.glade
          self.haskellPackages.ghcid
        ];
      in

      if indexTermonad
        then
          termonadEnv.overrideAttrs (oldAttrs: {
            nativeBuildInputs =
              let
                ghcEnvWithTermonad =
                  self.termonadKnownWorkingHaskellPkgSet.ghcWithHoogle (hpkgs: [ hpkgs.termonad ]);
              in
              oldAttrs.nativeBuildInputs ++ convenientNativeBuildTools ++ [ ghcEnvWithTermonad ];
          })
        else
          self.termonadKnownWorkingHaskellPkgSet.shellFor {
            withHoogle = true;
            packages = hpkgs: [ hpkgs.termonad ];
            nativeBuildInputs = termonadEnv.nativeBuildInputs ++ convenientNativeBuildTools;
          };
  };

in import nixpkgsSrc { overlays = [ haskellPackagesOverlay ] ++ additionalOverlays; }
