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
          # Recent version of nixpkgs master as of 2021-05-09 which uses nightly-2020-05-07.
          url = "https://github.com/NixOS/nixpkgs/archive/1c16013bd6e94da748b41cc123c6b509a23eb440.tar.gz";
          sha256 = "1m2wif0qnci0q14plbqlb95vx214pxqgw5li86lyw6hsiy7y3zfn";
        }
      else nixpkgs;

  compilerVersion = if isNull compiler then "ghc8104" else compiler;

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
                  (path: type: with self.lib;
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
                    # There are Haskell packages called gtk3 and pcre2, which
                    # makes these system dependencies not able to be resolved
                    # correctly.
                    inherit (self) gtk3 pcre2;
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
          self.hlint
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
