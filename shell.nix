# Running `nix-shell` in the same directory as this file will enter an
# environment in which you can easily hack on or build termonad. It provides:
#  * Termonad's system dependencies.
#  * GHC with all of termonad's haskell dependencies.
#  * Local haddocks for those dependencies.
#  * Hoogle with an index generated from those haddocks.
#  * Cabal.
#  * ghcid.
#  * open-haddock.
#
# In this environment you should be able to e.g.
#  * Build termonad:
#    $ cabal new-build
#  * Open a repl with access to the termonad libraries:
#    $ cabal new-repl
#  * Use `ghcid`:
#    $ ghcid --command='cabal new-repl'
#  * Open local haddocks for e.g. GI.Vte.Objects.Terminal:
#    $ open-haddock GI.Vte.Objects.Terminal
#    or for the gi-gtk package:
#    $ open-haddock gi-gtk
#
# If you pass nix-shell the arguments `--arg indexTermonad true`, then hoogle
# will also index the Termonad libraries, however this will mean the environment
# will need to be rebuilt every time the termonad source changes.

{ compiler ? "ghc843", indexTermonad ? false }:

let
  nixpkgs = (import .nix-helpers/nixpkgs.nix { inherit compiler; });
  termonadEnv = nixpkgs.haskellPackages.termonad.env;
  nativeBuildTools = with nixpkgs.haskellPackages; [ cabal-install ghcid open-haddock ];
in

if indexTermonad
  then
    termonadEnv.overrideAttrs (oldAttrs: {
      nativeBuildInputs =
        oldAttrs.nativeBuildInputs ++
        nativeBuildTools ++
        [ (nixpkgs.haskellPackages.ghcWithHoogle (haskellPackages: with haskellPackages; [ termonad ]))
        ];
    })
  else
    nixpkgs.haskellPackages.shellFor {
      withHoogle = true;
      packages = haskellPackages: [ haskellPackages.termonad ];
      nativeBuildInputs = termonadEnv.nativeBuildInputs ++ nativeBuildTools;
    }
