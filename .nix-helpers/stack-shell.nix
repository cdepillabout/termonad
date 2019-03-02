# This is the shell file specified in the stack.yaml file.
# This runs stack commands in an environment created with nix.

let
  # recent version of nixpkgs master as of 2019-03-02
  nixpkgsTarball = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/07e2b59812de95deeedde95fb6ba22d581d12fbc.tar.gz";
    sha256 = "1yxmv04v2dywk0a5lxvi9a2rrfq29nw8qsm33nc856impgxadpgf";
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
