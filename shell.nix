
let
  nixpkgsTarball = builtins.fetchTarball {
    # 17.09 (this works)
    # url = "https://github.com/NixOS/nixpkgs/archive/39cd40f7bea40116ecb756d46a687bfd0d2e550e.tar.gz";
    # sha256 = "0kpx4h9p1lhjbn1gsil111swa62hmjs9g93xmsavfiki910s73sh";
    # 18.03 (this works)
    url = "https://github.com/NixOS/nixpkgs/archive/120b013e0c082d58a5712cde0a7371ae8b25a601.tar.gz";
    sha256 = "0hk4y2vkgm1qadpsm4b0q1vxq889jhxzjx3ragybrlwwg54mzp4f";
    # recent version of nixpkgs (as of 2018-07-19)
    # url = "https://github.com/NixOS/nixpkgs/archive/d7d31fea7e7eef8ff4495e75be5dcbb37fb215d0.tar.gz"
    # sha256 = "013na1m4g8c3rcfw0dwmv2zmia6byg5c2xdx3z5dk90i27s449kx";
  };
  nixpkgs = import nixpkgsTarball { };

  fixed-stack-repo = import ./.nix-helpers/fixed-stack.nix { inherit nixpkgs; };

  myStack = fixed-stack-repo.working.stack;
in

with nixpkgs;

# let R = pkgs.R.override { enableStrictBarrier = true; };
# in
haskell.lib.buildStackProject {
  name = "termonad";
  buildInputs = [
    cairo
    gnome3.vte
    gobjectIntrospection
    gtk3
    zlib
    # Some of these are probably not actually needed...
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
  ghc = haskell.compiler.ghc802;
  extraArgs = [
    "--stack-yaml stack-lts-9.yaml"
  ];
  # stack = myStack;
}
