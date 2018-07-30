
let
  nixpkgsTarball = builtins.fetchTarball {
    # recent version of nixpkgs as of 2018-07-29
    url = "https://github.com/NixOS/nixpkgs/archive/a2c6dbe370160ffea5537f64dda04489184c5ce1.tar.gz";
    sha256 = "1x993g9343yv5wyp29i6vskdcc3rl42xipv79nwmmrj8ay2yhh3b";
  };
  nixpkgs = import nixpkgsTarball { };

  termonad = nixpkgs.callPackage ../. { };

  ghcStuff = nixpkgs.pkgs.haskell.packages.ghc843.ghcWithPackages (pkgs: [
    pkgs.colour
    pkgs.lens
    termonad
  ]);

in

nixpkgs.runCommand "dummy" { buildInputs = [ ghcStuff termonad ]; } ""
