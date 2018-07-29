{ mkDerivation, base, bytestring, Cabal, containers, gi-atk, gi-gdk
, gi-gio, gi-glib, gi-gobject, gi-gtk, gi-pango, gtk3, haskell-gi
, haskell-gi-base, haskell-gi-overloading, stdenv, text
, transformers, vte
}:
mkDerivation {
  pname = "gi-vte";
  version = "2.91.17";
  sha256 = "5c89651e73c1002c356be8d5b7364cb9e66d9825aab4b1d6ce9bcd1a30f754df";
  setupHaskellDepends = [ base Cabal haskell-gi ];
  libraryHaskellDepends = [
    base bytestring containers gi-atk gi-gdk gi-gio gi-glib gi-gobject
    gi-gtk gi-pango haskell-gi haskell-gi-base haskell-gi-overloading
    text transformers
  ];
  libraryPkgconfigDepends = [ gtk3 vte ];
  homepage = "https://github.com/haskell-gi/haskell-gi";
  description = "Vte bindings";
  license = stdenv.lib.licenses.lgpl21;
}
