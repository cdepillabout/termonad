# This is the shell file specified in the stack.yaml file.
# This runs stack commands in an environment created with nix.

let
  # recent version of nixpkgs master as of 2018-12-23
  nixpkgsTarball = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/c31c0558ddad7161a4025117694197264cda9750.tar.gz";
    sha256 = "09xl8fshyyddcm5nw5fkl6fbjlh5szjcdm43ii6jsvykdr516ghp";
  };

  # Fixes for individual packages.  Currently none needed.
  pkgFixes = self: pkgs: {
  };

  nixpkgs = import nixpkgsTarball {
    overlays = [
      pkgFixes
    ];
  };
in

with nixpkgs;

haskell.lib.buildStackProject {
  name = "termonad";
  buildInputs = [
    cairo
    git
    gnome3.vte
    gobjectIntrospection
    gtk3
    zlib
  ];
  ghc = haskell.compiler.ghc863;
  extraArgs = [
    "--stack-yaml stack-lts-13.yaml"
  ];
}
