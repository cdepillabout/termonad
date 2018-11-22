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
