{ mkDerivation, base, Cabal, cabal-doctest, classy-prelude, colour
, constraints, data-default, doctest, dyre, gi-gdk, gi-gio, gi-glib
, gi-gtk, gi-pango, gi-vte, haskell-gi-base, hedgehog, lens
, pretty-simple, QuickCheck, stdenv, tasty, tasty-hedgehog
, template-haskell, xml-conduit, xml-html-qq
}:
mkDerivation {
  pname = "termonad";
  version = "0.1.0.0";
  src = builtins.filterSource (path: type:
    type != "unknown" &&
    baseNameOf path != ".git" &&
    baseNameOf path != "result" &&
    baseNameOf path != ".stack-work" &&
    baseNameOf path != "dist") ./..;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    base classy-prelude colour constraints data-default dyre gi-gdk
    gi-gio gi-glib gi-gtk gi-pango gi-vte haskell-gi-base lens
    pretty-simple QuickCheck xml-conduit xml-html-qq
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base doctest hedgehog lens QuickCheck tasty tasty-hedgehog
    template-haskell
  ];
  homepage = "https://github.com/cdepillabout/termonad";
  description = "Terminal emulator configurable in Haskell";
  license = stdenv.lib.licenses.bsd3;
}
