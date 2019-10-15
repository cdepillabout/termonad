# This is a nix derivation that gives us a `stack` binary that will run in a
# chroot.  This is needed to workaround
# https://github.com/cdepillabout/termonad/issues/99.
#
# This is nice to use if you're having trouble running `stack` on nixos
# normally.  It generally shouldn't be needed any other time.
#
# You can use this by starting a `nix-shell` with it:
#
# $ nix-shell --pure .nix-helpers/stack-fhs-env.nix
#
# From here, you can run `stack` normally.

with (import ./nixpkgs.nix {});

# stack needs to be version 1.9.3, because versions greater than two can't be
# re-execed in a nix shell:
#
#https://github.com/commercialhaskell/stack/issues/5000

let
  nixpkgs-19-03-tarball = builtins.fetchTarball {
    # Channel nixos-19.03 as of 2019/08/12.
    url = "https://github.com/NixOS/nixpkgs/archive/56d94c8c69f8cac518027d191e2f8de678b56088.tar.gz";
    sha256 = "1c812ssgmnmh97sarmp8jcykk0g57m8rsbfjg9ql9996ig6crsmi";
  };

  nixpkgs-19-03 = import nixpkgs-19-03-tarball {};

  stack = nixpkgs-19-03.stack;

  fhsStack =
    buildFHSUserEnv {
      name = "stack";
      runScript = "stack";
      targetPkgs = pkgs: (with pkgs; [
          binutils
          cairo
          cairo.dev
          git
          gnome3.atk
          gnome3.gdk_pixbuf
          gnome3.glib
          gnome3.gtk
          gnome3.vte
          gnutls
          gobjectIntrospection
          gtk3
          iana-etc
          pango
          pcre2
          pkgconfig
          # stack
          termonadKnownWorkingHaskellPkgSet.ghc
          zlib
      ]) ++ [
        stack
      ] ++
        stdenv.lib.optional
          (stdenv.hostPlatform.libc == "glibc")
          glibcLocales;
      profile = ''
        export STACK_IN_NIX_SHELL=1
        export GI_TYPELIB_PATH=/usr/lib/girepository-1.0
        export XDG_DATA_DIRS=/usr/share:$XDG_DATA_DIRS
      '';
      extraOutputsToInstall = ["dev"];
    };
in

mkShell {
  buildInputs = [
    fhsStack
    gitAndTools.gitFull
    gitAndTools.hub
  ];
}
