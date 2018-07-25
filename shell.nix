
let
  nixpkgsTarball = builtins.fetchTarball {
    # 17.09 (this works)
    #url = "https://github.com/NixOS/nixpkgs/archive/39cd40f7bea40116ecb756d46a687bfd0d2e550e.tar.gz";
    #sha256 = "0kpx4h9p1lhjbn1gsil111swa62hmjs9g93xmsavfiki910s73sh";

    # 18.03 (this works)
    #url = "https://github.com/NixOS/nixpkgs/archive/120b013e0c082d58a5712cde0a7371ae8b25a601.tar.gz";
    #sha256 = "0hk4y2vkgm1qadpsm4b0q1vxq889jhxzjx3ragybrlwwg54mzp4f";

    # recent version of nixpkgs as of 2018-07-25.
    url = "https://github.com/NixOS/nixpkgs/archive/4ccaa7de8eb34a0bb140f109a0e88095480118eb.tar.gz";
    sha256 = "0svqfijh51g62k030dmhsn736pmbnb20baanmapn5wqkn9352nn4";
  };
  nixpkgs = import nixpkgsTarball { };

  # fixed-stack-repo = import ./.nix-helpers/fixed-stack.nix { inherit nixpkgs; };

  # myStack = fixed-stack-repo.working.stack;
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
  ];
  ghc = haskell.compiler.ghc843;
  extraArgs = [
    "--stack-yaml stack-lts-12.yaml"
  ];
  # stack = myStack;
}
