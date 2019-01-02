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

let
  # recent version of nixpkgs master as of 2018-12-23
  nixpkgsTarball = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/c31c0558ddad7161a4025117694197264cda9750.tar.gz";
    sha256 = "09xl8fshyyddcm5nw5fkl6fbjlh5szjcdm43ii6jsvykdr516ghp";
  };

  # Additional packages or fixes for individual packages.
  pkgOverlay = self: pkgs: {
    fhsStack =
      self.buildFHSUserEnv {
        name = "stack";
        runScript = "stack";
        targetPkgs = _: with self; [
            binutils
            cairo
            ghc
            git
            gnome3.atk
            gnome3.gdk_pixbuf
            gnome3.glib
            gnome3.gtk
            gnome3.vte
            gnutls
            gobjectIntrospection
            gtk3
            haskell.compiler.ghc863
            iana-etc
            pango
            pcre2
            pkgconfig
            stack
            zlib
        ] ++
          self.stdenv.lib.optional
            (self.stdenv.hostPlatform.libc == "glibc")
            self.glibcLocales;
        profile = ''
          export STACK_IN_NIX_SHELL=1
          export GI_TYPELIB_PATH=/usr/lib/girepository-1.0
        '';
        extraOutputsToInstall = ["dev"];
      };
  };

  nixpkgs = import nixpkgsTarball {
    overlays = [
      pkgOverlay
    ];
  };

in

with nixpkgs;

mkShell {
  buildInputs = [
    fhsStack
    gitAndTools.gitFull
    gitAndTools.hub
  ];
}
