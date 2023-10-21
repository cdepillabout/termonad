
# This is a Nix shell.nix that allows you to build Termonad in an environment
# similar to someone who gets cabal-install and GHC from ghcup.
#
# This enviroment includes the required system packages, as well as build tools
# like GHC and cabal-install, but it does not provide Haskell packages built
# with Nix.  You will need to allow cabal to build all Haskell packages when
# running `cabal build`.
#
#
# A couple different ways to use this shell.nix file:
#
# * By default, this shell.nix uses the nixpkgs repo defined in ./nixpkgs.nix,
#   but it is possible to pass in a different nixpkgs repo:
#
#   $ nix-shell ./.nix-helpers/simple-cabal-shell.nix --arg nixpkgs ~/some/path/to/nixpkgs
#
# * You can also pass a fully-imported nixpkgs if that is easier:
#
#   $ nix-shell ./.nix-helpers/simple-cabal-shell.nix --arg nixpkgs 'import <nixpkgs> {}'
#
# * By default, this shell.nix uses the top-level ghc attribute from the
#   passed-in Nixpkgs, but you can force a different GHC derivation to be used:
#
#   $ nix-shell ./.nix-helpers/simple-cabal-shell.nix --arg ghc 'with import <nixpkgs> {}; haskell.compiler.ghc94'
#
# * It may be easier to pass in a string of the GHC version to use:
#
#   $ nix-shell ./.nix-helpers/simple-cabal-shell.nix --argstr ghc-version ghc92
#
#
# In this environment you should be able to e.g.
#
# * Build termonad:
#
#   $ cabal new-build
#
# * Open a repl with access to the termonad libraries:
#
#   $ cabal new-repl

{ # Either a path to Nixpkgs, a fully-imported Nixpkgs, or null to use the default Nixpkgs.
  nixpkgs ? null
, # A derivation for the cabal-install package to use.  Normally it is okay to
  # just get this from nixpkgs that has been passed-in.
  cabal-install ? null
, # A derivation for the ghc to use.
  ghc ? null
, # A string for a derivation name for ghc to use from Nixpkgs.
  # Example: "ghc945"
  ghc-version ? null
}:

# Make sure that at least one of the ghc and ghc-version arguments is null. That is to say, they both can't be non-null.
assert ! (builtins.isNull ghc) -> builtins.isNull ghc-version;
assert ! (builtins.isNull ghc-version) -> builtins.isNull ghc;
assert ! (builtins.isNull ghc-version) -> builtins.isString ghc-version;

let
  pkgs =
    # If nixpkgs is a path, then just import it.
    if builtins.isPath nixpkgs then
      import nixpkgs {}
    # If nixpkgs is an attrset, then we assume it is a fully-imported Nixpkgs.
    else if builtins.isAttrs nixpkgs then
      nixpkgs
    # If nixpkgs is null, then just use the default Nixpkgs from this repo.
    else if builtins.isNull nixpkgs then
      import ./nixpkgs.nix {}
    # # It is an error to pass Nixpkgs as a string.
    else if builtins.isString nixpkgs then
      builtins.throw "nixpkgs argument can't be a string.  Did you mean to use `--arg nixpkgs ../some/path/to/nixpkgs` instead of `--argstr nixpkgs`?"
    else
      builtins.throw "nixpkgs argument must be either a path, a fully-imported nixpkgs, or null.  Did you mean to use `--arg nixpkgs ../some/path/to/nixpkgs`?";

  ghc-real =
    # If both the ghc and ghc-version arguments are null, then just use the default ghc from the Nixpkgs we got.
    if builtins.isNull ghc && builtins.isNull ghc-version then
      pkgs.ghc
    # If the ghc argument is null, then try to pull the given ghc-version from Nixpkgs.
    else if builtins.isNull ghc then
      pkgs.haskell.compiler.${ghc-version}
    # If only the ghc-version argument is null, then just use the ghc that has been passed in.
    else if builtins.isAttrs ghc then
      ghc
    else
      builtins.throw "ghc argument must be either be null, or a ghc derivation. Or the ghc-version argument can be specified as a string like \"ghc945\".";

  cabal-install-real =
    if builtins.isAttrs cabal-install then
      cabal-install
    else if builtins.isNull cabal-install then
      pkgs.cabal-install
    else
      builtins.throw "cabal-install argument must be either be null, or a cabal-install derivation.";
in

pkgs.mkShell {
  name = "simple-cabal-shell";
  packages = [
    # GHC and cabal-install for dev
    ghc-real
    cabal-install-real

    # pkgconfig for linking pkgconfig deps
    pkgs.pkgconfig

    # Required system libraries
    pkgs.gobject-introspection
    pkgs.libdatrie
    pkgs.libepoxy
    pkgs.libselinux
    pkgs.libsepol
    pkgs.libthai
    pkgs.libxkbcommon
    pkgs.pcre
    pkgs.pcre2
    pkgs.util-linux
    pkgs.vte
    pkgs.xorg.libXdmcp
    pkgs.xorg.libxcb
    pkgs.xorg.libXtst
    pkgs.zlib
  ];
}
