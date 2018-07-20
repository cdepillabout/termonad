{ nixpkgs, compiler ? "ghc843" }:

let
  # TODO: Remove when https://github.com/NixOS/cabal2nix/pull/360 is merged and available
  cabal2nix-fix-overlay = final: previous:
    with final.haskell.lib; {
      haskellPackages = previous.haskellPackages.override (old: {
        overrides = final.lib.composeExtensions (old.overrides or (_: _: {})) (

          self: super: {
            cabal2nix = overrideCabal super.cabal2nix (old: {
              src = pkgs.fetchFromGitHub {
                owner = "nh2";
                repo = "cabal2nix";
                rev = "5721bed2a598a018119413bfe868bd286735cb15";
                sha256 = "1436ri6nlfcgd263byb596dcx6g4l9fx47hm11vfh34x849r2kcy";
              };
            });

          }
        );
      });
    };

  nh2-nixpkgs = nixpkgs.fetchFromGitHub {
    owner = "nh2";
    repo = "nixpkgs";
    rev = "925aac04f4ca58aceb83beef18cb7dae0715421b";
    sha256 = "1zr8lscjl2a5cz61f0ibyx55a94v8yyp6sjzjl2gkqjrjbg99abx";
  };

  pkgs = (import nh2-nixpkgs {
    overlays = [ cabal2nix-fix-overlay ];
  }).pkgsMusl;

  normalHaskellPackages = pkgs.haskellPackages;

  sqlite_static = pkgs.sqlite.overrideAttrs (old: { dontDisableStatic = true; });

  lzma_static = pkgs.lzma.overrideAttrs (old: { dontDisableStatic = true; });

  haskellPackages = with pkgs.haskell.lib; normalHaskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: {})) (self: super:
    let
      # TODO do this via patches instead
      cabal_patched_src = pkgs.fetchFromGitHub {
        owner = "nh2";
        repo = "cabal";
        rev = "748f07b50724f2618798d200894f387020afc300";
        sha256 = "1k559m291f6spip50rly5z9rbxhfgzxvaz64cx4jqpxgfhbh2gfs";
      };

      Cabal_patched_Cabal_subdir = pkgs.stdenv.mkDerivation {
        name = "cabal-dedupe-src";
        buildCommand = ''
          cp -rv ${cabal_patched_src}/Cabal/ $out
        '';
      };

      Cabal_patched = self.callCabal2nix "Cabal" Cabal_patched_Cabal_subdir {};

      useFixedCabal = drv: overrideCabal drv (old: {
        setupHaskellDepends = (if old ? setupHaskellDepends then old.setupHaskellDepends else []) ++ [ Cabal_patched ];
        # TODO Check if this is necessary
        libraryHaskellDepends = (if old ? libraryHaskellDepends then old.libraryHaskellDepends else []) ++ [ Cabal_patched ];
      });

      statify = drv: with pkgs.haskell.lib; pkgs.lib.foldl appendConfigureFlag (disableLibraryProfiling (disableSharedExecutables (useFixedCabal drv))) [
        # "--ghc-option=-fPIC"
        "--enable-executable-static" # requires `useFixedCabal`
        "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
        # TODO These probably shouldn't be here but only for packages that actually need them
        "--extra-lib-dirs=${pkgs.zlib.static}/lib"
        "--extra-lib-dirs=${pkgs.ncurses.override { enableStatic = true; }}/lib"
      ];

    in
    {
      # Helpers for other packages

      hpc-coveralls = appendPatch super.hpc-coveralls (builtins.fetchurl https://github.com/guillaume-nargeot/hpc-coveralls/pull/73/commits/344217f513b7adfb9037f73026f5d928be98d07f.patch);
      persistent-sqlite = super.persistent-sqlite.override { sqlite = sqlite_static; };
      lzma = super.lzma.override { lzma = lzma_static; };
      # If we `useFixedCabal` on stack, we also need to use the
      # it on hpack and hackage-security because otherwise
      # stack depends on 2 different versions of Cabal.
      hpack = useFixedCabal super.hpack;
      hackage-security = useFixedCabal super.hackage-security;

      # Static executables that work

      hello = statify super.hello;
      hlint = statify super.hlint;
      ShellCheck = statify super.ShellCheck;
      cabal-install = statify super.cabal-install;
      bench = statify super.bench;

      stack = useFixedCabal (statify super.stack);

      dhall = statify super.dhall;

      cachix = appendConfigureFlag (statify super.cachix) [ "--ghc-option=-j1" ];

      # Static executables that don't work yet

    });
  });

in
  rec {
    working = {
      inherit (haskellPackages)
        hello
        stack
        hlint
        ShellCheck
        cabal-install
        bench
        dhall
        cachix
        ;
    };

    notWorking = {
      inherit (haskellPackages)
        ;
    };

    all = working // notWorking;

    inherit haskellPackages;
  }

# TODO Update README to depend on nixpkgs master in use (instead of nh2's fork), and write something that picks nh2's patches I use on top here
# TODO Instead of picking https://github.com/NixOS/nixpkgs/pull/43713, use a Python script to dedupe `-L` flags from the NIX_*LDFLAGS variables
