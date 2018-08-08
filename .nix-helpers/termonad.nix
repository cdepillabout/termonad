{ mkDerivation, base, Cabal, cabal-doctest, classy-prelude, colour
, constraints, data-default, doctest, dyre, gi-gdk, gi-gio, gi-glib
, gi-gtk, gi-pango, gi-vte, gtk3, haskell-gi-base, hedgehog, lens
, pretty-simple, QuickCheck, stdenv, tasty, tasty-hedgehog
, template-haskell, xml-conduit, xml-html-qq
}:
mkDerivation {
  pname = "termonad";
  version = "0.2.0.0";
  src = builtins.filterSource (path: type:
    baseNameOf path != ".git" &&
    baseNameOf path != "result" &&
    baseNameOf path != ".stack-work" &&
    baseNameOf path != "dist") ./..;
  isLibrary = true;
  isExecutable = true;
  doCheck = false;
  enableSeparateDataOutput = true;
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    base classy-prelude colour constraints data-default dyre gi-gdk
    gi-gio gi-glib gi-gtk gi-pango gi-vte haskell-gi-base lens
    pretty-simple QuickCheck xml-conduit xml-html-qq
  ];
  libraryPkgconfigDepends = [ gtk3 ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base doctest hedgehog lens QuickCheck tasty tasty-hedgehog
    template-haskell
  ];
  homepage = "https://github.com/cdepillabout/termonad";
  description = "Terminal emulator configurable in Haskell";
  license = stdenv.lib.licenses.bsd3;
}
