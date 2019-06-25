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

let
  fhsStack =
    buildFHSUserEnv {
      name = "stack";
      runScript = "stack";
      targetPkgs = pkgs: with pkgs; [
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
          stack
          termonadKnownWorkingHaskellPkgSet.ghc
          zlib
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
