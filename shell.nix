
let
  nixpkgsTarball = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/dae9cf6106da19f79a39714f183ed253c62b32c5.tar.gz";
    sha256 = "0r3c00m96ldb9z81ay7vj8gnpk4bf8gjcdiad7mgxvwxr9ndskjx";
  };
  nixpkgs = import nixpkgsTarball { };
in

with nixpkgs;

# let R = pkgs.R.override { enableStrictBarrier = true; };
# in
haskell.lib.buildStackProject {
  name = "termonad";
  buildInputs = [
    gnome3.vte
    gobjectIntrospection
    gtk3
    zlib
    # Some of these are probably not actually needed...
    # cairo
    # gcc
    # glib
    # gnome2.gtk
    # gnome2.pango
    # gnome2.vte
    # gnome3.gtk
    # gnome3.vte
    # gobjectIntrospection
    # gtk3
    # pkgconfig
    # zlib
  ];
  ghc = haskell.compiler.ghc843;
}
