{ mkDerivation, base, Cabal, cabal-doctest, classy-prelude, colour
, constraints, data-default, doctest, dyre, gi-gdk, gi-gio, gi-glib
, gi-gtk, gi-pango, gi-vte, gtk3, haskell-gi-base, hedgehog, lens
, pretty-simple, QuickCheck, stdenv, tasty, tasty-hedgehog
, template-haskell, type-combinators, xml-conduit, xml-html-qq
}:
mkDerivation {
  pname = "termonad";
  version = "0.2.1.0";
  src = builtins.filterSource (path: type: with stdenv.lib;
    ! elem (baseNameOf path) [ ".git" "result" ".stack-work" ] &&
    ! any (flip hasPrefix (baseNameOf path)) [ "dist" ".ghc" ]
  ) ./..;
  isLibrary = true;
  isExecutable = true;
  doCheck = false;
  enableSeparateDataOutput = true;
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    base classy-prelude colour constraints data-default dyre gi-gdk
    gi-gio gi-glib gi-gtk gi-pango gi-vte haskell-gi-base lens
    pretty-simple QuickCheck type-combinators xml-conduit xml-html-qq
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
