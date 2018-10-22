{ mkDerivation, adjunctions, base, Cabal, cabal-doctest
, classy-prelude, colour, constraints, data-default, directory
, distributive, doctest, dyre, filepath, genvalidity-hspec, gi-gdk
, gi-gio, gi-glib, gi-gtk, gi-pango, gi-vte, gtk3, haskell-gi-base
, hedgehog, lens, pretty-simple, QuickCheck, singletons, stdenv
, tasty, tasty-hedgehog, tasty-hspec, template-haskell, xml-conduit
, xml-html-qq
}:
mkDerivation {
  pname = "termonad";
  version = "0.2.1.0";
  src = ./..;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    adjunctions base classy-prelude colour constraints data-default
    directory distributive dyre filepath gi-gdk gi-gio gi-glib gi-gtk
    gi-pango gi-vte haskell-gi-base lens pretty-simple QuickCheck
    singletons xml-conduit xml-html-qq
  ];
  libraryPkgconfigDepends = [ gtk3 ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base doctest genvalidity-hspec hedgehog lens QuickCheck tasty
    tasty-hedgehog tasty-hspec template-haskell
  ];
  homepage = "https://github.com/cdepillabout/termonad";
  description = "Terminal emulator configurable in Haskell";
  license = stdenv.lib.licenses.bsd3;
}
