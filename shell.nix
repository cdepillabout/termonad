# Running `nix-shell` in the same directory as this file will enter an
# environment in which you can easily hack on or build termonad. It provides:
#  * Termonad's system dependencies.
#  * GHC with all of termonad's haskell dependencies.
#  * Local haddocks for those dependencies.
#  * Hoogle with an index generated from those haddocks.
#  * Cabal.
#
# In this environment you should be able to e.g.
#  * Build termonad by running `cabal new-build`.
#  * Open a repl with access to the termonad libraries by running
#    `cabal new-repl`.
#  * Use `ghcid` (not provided), by running `ghcid --command='cabal new-repl'`.
#  * Open local haddocks with the open-haddock utility (not provided; see
#    github.com/jml/open-haddock for details).
#
# If you pass nix-shell the arguments `--arg indexTermonad true`, then hoogle
# will also index the Termonad libraries, however this will mean the environment
# will need to be rebuilt every time the termonad source changes.

{ compiler ? "ghc843", indexTermonad ? false }:

let

  hspkgs =
    (import .nix-helpers/nixpkgs.nix { inherit compiler; }).haskellPackages;
  termonad = import .nix-helpers/bare.nix { inherit compiler; };

  addNativeBIs = drv: nbis: drv.overrideAttrs (oa: {
    nativeBuildInputs = oa.nativeBuildInputs ++ nbis ;
  });

in

if indexTermonad
  then
    addNativeBIs termonad.env [
      hspkgs.cabal-install
      (hspkgs.ghcWithHoogle (_: [ termonad ]))
    ]
  else
    hspkgs.shellFor {
      withHoogle = true;
      packages = _: [ termonad ];
      nativeBuildInputs = termonad.env.nativeBuildInputs ++ [
        hspkgs.cabal-install
      ];
    }
