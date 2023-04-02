# This is the shell file specified in the stack.yaml file.
# This runs stack commands in an environment created with nix.

with (import ./nixpkgs.nix {});

haskell.lib.buildStackProject {
  name = "termonad";
  buildInputs = [
    cairo
    git
    gobject-introspection
    gtk3
    vte
    zlib
  ];
  ghc = termonadKnownWorkingHaskellPkgSet.ghc;
}
