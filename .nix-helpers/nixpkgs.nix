# This file pins the version of nixpkgs to a known good version. The nixpkgs is
# imported with an overlay adding Termonad. It is imported from various other
# files.

{ # String representing a GHC version to use.  Normally something like
  # "ghc865".  If null, then use a known-working GHC version.
  compiler ? null
, # A path to nixpkgs.  This will be imported.  If null, use a known-working
  # nixpkgs version.
  nixpkgs ? null
, # Additional overlays to apply when importing nixpkgs.
  additionalOverlays ? []
}:

let
  nixpkgsSrc =
    if isNull nixpkgs
      then
        builtins.fetchTarball {
          # Recent version of nixpkgs-19.03 as of 2019-06-25.
          url = "https://github.com/NixOS/nixpkgs/archive/8634c3b619909db7fc747faf8c03592986626e21.tar.gz";
          sha256 = "sha256:0hcpy4q64vbqmlmnfcavfxilyygyzpwdsss8g3p73ikpic0j6ziq";
        }
      else nixpkgs;

  compilerVersion = if isNull compiler then "ghc865" else compiler;

  # An overlay that adds termonad to all haskell package sets.
  haskellPackagesOverlay = self: super: {
    haskell = super.haskell // {
      packageOverrides = hself: hsuper:
        super.haskell.packageOverrides hself hsuper // {
          termonad =
            let
              src =
                builtins.filterSource
                  (path: type: with self.stdenv.lib;
                    ! elem (baseNameOf path) [ ".git" "result" ".stack-work" ".nix-helpers" ] &&
                    ! any (flip hasPrefix (baseNameOf path)) [ "dist" ".ghc" ]
                  )
                  ./..;
            in
            hself.callCabal2nix "termonad" src {
              inherit (self.gnome3) gtk3;
              vte_291 = self.gnome3.vte;
            };
        };
    };

    termonadKnownWorkingHaskellPkgSet = self.haskell.packages.${compilerVersion};
  };

in import nixpkgsSrc { overlays = [ haskellPackagesOverlay ] ++ additionalOverlays; }
