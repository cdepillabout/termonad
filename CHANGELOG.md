## (next)

* Fix up deprecated functions used in Setup.hs.  This should allow Termonad to
  be compiled with Cabal-3.0.0.0 (which is used by default in GHC-8.8).
  [#144](https://github.com/cdepillabout/termonad/pull/144) Thanks
  [mdorman](https://github.com/mdorman)!

## 3.0.0.0

* Remove the one-pixel white border around the `GtkNotebook` (the GTK widget thing
  that contains the tabs). [#138](https://github.com/cdepillabout/termonad/pull/138)
* Add a right-click menu for the terminal.  It currently allows copy and
  paste.  [#136](https://github.com/cdepillabout/termonad/pull/136)  Thanks
  @jecaro!
* Add a preferences file that settings will be saved to and read from at
  `~/.config/termonad/termonad.yaml`.  You can change settings with the
  Preferences dialog.  **The settings will only be used from this file if you
  do not have a `~/.config/termonad/termonad.hs` file**.
  [#140](https://github.com/cdepillabout/termonad/pull/140) Thanks again
  @jecaro!

## 2.1.0.0

* Add a menu option to set preferences for a running Termonad session.  The preferences you have set are lost when you end the Termonad session. [#130](https://github.com/cdepillabout/termonad/pull/130)  Thanks @jecaro!

## 2.0.0.0

* Added menu option to search for a regex within the terminal output.
  This removes support for versions of VTE-2.91 older than 0.46.
  This means that compiling on older versions of Debian and Ubuntu may no
  longer work. [#118](https://github.com/cdepillabout/termonad/pull/118)

## 1.3.0.0

* Change all uses of
  [`Colour`](http://hackage.haskell.org/package/colour-2.3.5/docs/Data-Colour.html#t:Colour)
  to
  [`AlphaColour`](http://hackage.haskell.org/package/colour-2.3.5/docs/Data-Colour.html#t:AlphaColour)
  in `Termonad.Config.Colour`.  Users should now use `AlphaColour` instead of
  `Colour`.  Also, all uses of `sRGB24` should be replaced with `createColour`.
  This change is mechanical and should not affect how Termonad works at all.
  Thanks to @jecaro and @amir! [#116](https://github.com/cdepillabout/termonad/pull/116)

## 1.2.0.0

* Got the code for setting the backgroud color of the terminal actually
  working.  Thanks @dakotaclemenceplaza.
  [#111](https://github.com/cdepillabout/termonad/pull/111)
  * This changes the type of `ColourConfig` to make the foreground and
    background colors of the terminal optional.
* Various cleanup in the nix files.

## 1.1.0.0

* Added an
  [example](https://github.com/cdepillabout/termonad/blob/0cd741d51958806092418b55abdf1c1dc078841c/example-config/ExampleSolarizedColourExtension.hs)
  of how to setup a solarized color scheme. Thanks @craigem.
  [#90](https://github.com/cdepillabout/termonad/pull/90) and [#103](https://github.com/cdepillabout/termonad/pull/103)
* Various fixes in the nix files.
  * Make sure Termonad can see the GTK icons.
    [#91](https://github.com/cdepillabout/termonad/pull/91) and
    [#92](https://github.com/cdepillabout/termonad/pull/92)
* Add a menu option to change the font size at runtime.  You should be able to
  do this with the `Ctrl-+` and `Ctrl--` keys.
  [#95](https://github.com/cdepillabout/termonad/pull/95)
* Get building with GHC 8.6. Thanks @clinty. [#98](https://github.com/cdepillabout/termonad/pull/98)

## 1.0.1.0

* Stop using the `widgetSetFocusOnClick` function, which is not supported on
  older versions of GTK. This lets Termonad be compiled with older versions
  of GTK. [#87](https://github.com/cdepillabout/termonad/pull/87).
* Add CI. [#87](https://github.com/cdepillabout/termonad/pull/87).
* Support versions of VTE-2.91 older than 0.44.
  [#87](https://github.com/cdepillabout/termonad/pull/87).
* Add some functions for converting from a list to a `Vec` in
  `Termonad.Config.Vec`: `fromListVec` and `fromListVec_`.  Commit 883eb98b5f.
* Fix the paste hotkey. [#86](https://github.com/cdepillabout/termonad/pull/86).

## 1.0.0.0

* The API for configuring Termonad is now completely different. Many, many
  changes have gone into this version.  You should approach it as a
  completely different application.

  The CHANGELOG will be kept up-to-date for future releases.

## 0.2.1.0

* Make sure the window title is set to "Termonad".
* Relabel tabs when termonad is started.

## 0.2.0.0

* Open dialog asking if you want to quit when you try to use your WM to quit.
* Termonad will attempt to open up a new terminal in the working directory of
  the current terminal.
* Make sure termonad won't crash if dyre can't find GHC.
* Add a few more ways to compile on NixOS.
* Add an icon for termonad.

## 0.1.0.0

* Initial release.
