{ compiler ? "ghc843" }:
(import ./default.nix { inherit compiler; }).env
