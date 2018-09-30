# This is the shell file specified in the stack.yaml file.
# This forces stack to use ghc-8.0.2 and stack-lts-9.yaml to compile termonad.

let
  nixpkgsTarball = builtins.fetchTarball {
    # 17.09 (this works)
    #url = "https://github.com/NixOS/nixpkgs/archive/39cd40f7bea40116ecb756d46a687bfd0d2e550e.tar.gz";
    #sha256 = "0kpx4h9p1lhjbn1gsil111swa62hmjs9g93xmsavfiki910s73sh";

    # 18.03
    url = "https://github.com/NixOS/nixpkgs/archive/120b013e0c082d58a5712cde0a7371ae8b25a601.tar.gz";
    sha256 = "0hk4y2vkgm1qadpsm4b0q1vxq889jhxzjx3ragybrlwwg54mzp4f";

    # recent version of nixpkgs as of 2018-07-25 (this only seems to sometimes work...?))
    #url = "https://github.com/NixOS/nixpkgs/archive/4ccaa7de8eb34a0bb140f109a0e88095480118eb.tar.gz";
    #sha256 = "0szbxfrzmlmxrgkqz5wnfgmsjp82vaddgz7mhdz7jj0jhd0hza4i";
  };

  pkgFixes = self: pkgs: {
    gtk3 = pkgs.gtk3.overrideDerivation (oldattrs: {
      patches = oldattrs.patches ++ [ ./patches/gdk.patch ];
    } );
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
    gnome3.vte
    gobjectIntrospection
    gtk3
    zlib
  ];
  ghc = haskell.compiler.ghc802;
  extraArgs = [
    "--stack-yaml stack-lts-9.yaml"
  ];
}
