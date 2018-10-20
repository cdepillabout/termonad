# This is the shell file specified in the stack.yaml file.
# This runs stack commands in an environment created with nix.

let
  # recent version of nixpkgs as of 2018-10-17
  nixpkgsTarball = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/6a23e11e658b7a7a77f1b61d30d64153b46bc852.tar.gz";
    sha256 = "03n4bacfk1bbx3v0cx8xcgcmz44l0knswzh7hwih9nx0hj3x41yc";
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
  ghc = haskell.compiler.ghc843;
  extraArgs = [
    "--stack-yaml stack-lts-12.yaml"
  ];
}
