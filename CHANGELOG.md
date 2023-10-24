## (next)

*   Embed the Termonad icon into the Termonad library (instead of having it set
    as a `data-file` in `termonad.cabal`).

    This fixes a problem some users were seeing when garbaging-collecting their
    Nix store and losing the required `termonad-lambda.png` file that their
    Termonad binary was referencing:
    [#165](https://github.com/cdepillabout/termonad/issues/165)

    Thanks [@refaelsh](https://github.com/refaelsh)! [#236](https://github.com/cdepillabout/termonad/pull/236)

*   Add support for setting Termonad options with CLI arguments.  Add a whole
    CLI argument parser based on optparse-applicative.
    [#234](https://github.com/cdepillabout/termonad/pull/234)

    Check out `termonad --help` to see the options available to be set from the CLI.

    By default, passing CLI options will override options specified in the
    `termonad.hs` file, as well as options specified in the Preferences dialog.

    In your own `termonad.hs` file, if you want to not use this CLI argument
    funtionality, you should be able to use the `Termonad.start` function (in
    place of `Termonad.defaultMain`).

*   Rename the `Termonad.PreferencesFile` module to `Termonad.Preferences.File`.

    Also, add a `Termonad.Preferences` module that re-exports everything helpful
    from the `Termonad.Preferences.File` module.  Also, some of the
    preferences-related functionality from `Termonad.App` has been moved into
    `Termonad.Preferences`.
    [#238](https://github.com/cdepillabout/termonad/pull/238)

*   Move the `defaultMain` function from `Termonad.App` to `Termonad.Startup`.
    (It is also exported from the top-level `Termonad` module, so most users
    will likely want to get it from there.)
    [#239](https://github.com/cdepillabout/termonad/pull/239)


## 4.5.0.0

*   Add an `allowBold` option (which defaults to `True`).  This can be used if
    you want disable use of bold text.
    Thanks [@zanculmarktum](https://github.com/zanculmarktum)!
    [#225](https://github.com/cdepillabout/termonad/pull/225)

## 4.4.0.0

*   Add support for opening URLs in a browser by right-clicking on them.
    URLs will also become underlined if you mouse-over them.
    [#222](https://github.com/cdepillabout/termonad/pull/222)

## 4.3.0.0

*   Add SIXEL support.  Note that you will need to set `enableSixel` to `True`
    in your `ConfigOptions`. In order for `enableSixel` to have any affect,
    you'll need to use version of VTE that is >= 0.63, and has been compiled
    with SIXEL support. There is also a report that even if you enable SIXEL
    and have a supported version of VTE, there may still be some problems.  See
    the linked PR for more information, including how to compile VTE with SIXEL
    support. Thanks
    [@junjihashimoto](https://github.com/junjihashimoto)!
    [#219](https://github.com/cdepillabout/termonad/pull/219)

## 4.2.0.1

*   Added an
    [example](https://github.com/cdepillabout/termonad/blob/50b20dc3f5bb2a4a080fd703e818bf721c9c3884/example-config/ExampleDraculaColourExtension.hs)
    of how to setup a Dracula color scheme. Thanks @craigem!
    [#195](https://github.com/cdepillabout/termonad/pull/195)

*   Bump to allow both aeson-1 and aeson-2.
    Thanks [@gelisam](https://github.com/gelisam)!
    [#210](https://github.com/cdepillabout/termonad/pull/210)

## 4.2.0.0

*   Add new options `highlightFgColour` and `highlightBgColour` for setting
    the color of highlighted text
    [#190](https://github.com/cdepillabout/termonad/pull/190).
    Thanks [@zanculmarktum](https://github.com/zanculmarktum)!

*   Termonad creates a configuration file in `~/.config/termonad/termonad.yaml`
    for use with the Preferences editor.  This is only used if you don't
    have a `termonad.hs` file.

    The configuration file loading code has been updated to be more robust in
    loading configurations that are missing fields.  This means that if you
    update Termonad from an old version, your preferences will still be able to
    be loaded in most cases
    [#191](https://github.com/cdepillabout/termonad/pull/191).  Thanks again
    [@jecaro](https://github.com/jecaro)!

*   Added an
    [example](https://github.com/cdepillabout/termonad/blob/74d04ba1469184cd8667f88d24dfbc7d50d7f658/example-config/ExamplePaperColourColourExtension.hs)
    of how to setup a PaperColour color scheme. Thanks @craigem!
    [#193](https://github.com/cdepillabout/termonad/pull/193)

## 4.1.1.0

*   Add new shortcuts to switch to the next and previous tab:
    <kbd>Ctrl</kbd><kbd>PgDown</kbd> and <kbd>Ctrl</kbd><kbd>PgUp</kbd>.
    This works similar to gnome-terminal and xfce4-terminal.
    [#180](https://github.com/cdepillabout/termonad/pull/180).
    Thanks [@juliendehos](https://github.com/juliendehos)!

## 4.1.0.0

*   Add an option for enabling "bold is bright".  This forces colors from the
    extended light palette to be used whenever Termonad prints bold text.
    [#178](https://github.com/cdepillabout/termonad/pull/178).
    Thanks [@M0M097](https://github.com/M0M097)!

## 4.0.1.2

*   Disable doctest test-suite when building with GHC-8.10.3.  The doctests
    appear to be segfaulting, but only when compiled with GHC-8.10.3.
    [#175](https://github.com/cdepillabout/termonad/pull/175).

## 4.0.1.1

*   Bump upper dependency on `base` so that Termonad is compatible with
    GHC-8.10. [#172](https://github.com/cdepillabout/termonad/pull/172).
    Thanks [@mimi1vx](https://github.com/mimi1vx)!

## 4.0.1.0

*   Add Preferences link to context menu.  This is a convenient way to open the
    Preferences if you don't have the menu shown by default.
    [#171](https://github.com/cdepillabout/termonad/pull/171) Thanks
    [@maridonkers](https://github.com/maridonkers)!

## 4.0.0.1

*   Update Termonad to be able to be built with the latest versions of the
    haskell-gi libraries.  This shouldn't affect most users building with
    `stack`. It is only used
    [currently](https://github.com/NixOS/nixpkgs/pull/95434) for building
    Termonad with packages from Nixpkgs.

## 4.0.0.0

*   Remove the dependently typed code for specifying terminal colors.
    [#161](https://github.com/cdepillabout/termonad/pull/161).
    Thanks [@ssbothwell](https://github.com/ssbothwell)!

    The `Palette` data type has been updated to not used length-indexed lists,
    but instead just newtype wrappers around normal lists.

    In prevous versions, the `Palette` data type looked like this:

    ```haskell
    data Palette c
      = NoPalette
      | BasicPalette !(Vec N8 c)
      | ExtendedPalette !(Vec N8 c) !(Vec N8 c)
      | ColourCubePalette !(Vec N8 c) !(Vec N8 c) !(Matrix '[N6, N6, N6] c)
      | FullPalette !(Vec N8 c) !(Vec N8 c) !(Matrix '[N6, N6, N6] c) !(Vec N24 c)
    ```

    In 4.0.0.0, `Palette` has been changed to the following:

    ```haskell
    data Palette c
      = NoPalette
      | BasicPalette !(List8 c)
      | ExtendedPalette !(List8 c) !(List8 c)
      | ColourCubePalette !(List8 c) !(List8 c) !(Matrix c)
      | FullPalette !(List8 c) !(List8 c) !(Matrix c) !(List24 c)
    ```

    Instead of using types like `Vec N8 c`, you will use types like `List8 c`.

    When setting the `palette` field of in a `ColourConfig`, you can now do
    it like the following.  Note that there is both a `mkList8` function that
    returns `Maybe`, and an `unsafeMkList8` that throws a runtime error.
    Most users will probably want to use the `unsafeMkList8` function, since
    it is easy to use, and you can eyeball whether the list has the correct
    number of elements.  If you're doing something more complicated, you may
    want to use the `mkList8` function:

    ```haskell
    myColourConfig :: ColourConfig (AlphaColour Double)
    myColourConfig =
      defaultColourConfig
        { palette =
            ExtendedPalette
              myStandardColours (maybe defaultLightColours id myLightColours)
        }
      where
        -- This is a an example of creating a linked-list of colours,
        -- This function uses an unsafe method for generating the list.
        -- An exception will be thrown if your list does not have exactly 8 elements.
        myStandardColours :: List8 (AlphaColour Double)
        myStandardColours = unsafeMkList8
          [ createColour  40  30  20 -- dark brown (used as background colour)
          , createColour 180  30  20 -- red
          , createColour  40 160  20 -- green
          , createColour 180 160  20 -- dark yellow
          , createColour  40  30 120 -- dark purple
          , createColour 180  30 120 -- bright pink
          , createColour  40 160 120 -- teal
          , createColour 180 160 120 -- light brown
          ]

        -- This is an example of creating a linked-list of colours with a type
        -- safe method. mkList8 produces a Maybe value which must be handled explicitely.
        myLightColours :: Maybe (List8 (AlphaColour Double))
        myLightColours = mkList8
            [ createColour  70  60  50 -- brown
            , createColour 220  30  20 -- light red
            , createColour  40 210  20 -- light green
            , createColour 220 200  20 -- yellow
            , createColour  40  30 180 -- purple
            , createColour 140  30 80  -- dark pink
            , createColour  50 200 160 -- light teal
            , createColour 220 200 150 -- light brown
            ]
    ```

    Also see the functions `setAtList8`, `overAtList8`, `setAtList24`,
    `overAtList24`, etc.

## 3.1.0.1

*   Correct the solarized colours
    [#148](https://github.com/cdepillabout/termonad/pull/148).
    Thanks [@craigem](https://github.com/craigem)!

*   Add an example showing Gruvbox colours
    [#149](https://github.com/cdepillabout/termonad/pull/149).
    Thanks again [@craigem](https://github.com/craigem)!

*   Set an upperbound on `base` so we make sure that only GHC-8.8 is used.  Some
    of the dependencies of Termonad don't support GHC-8.10 yet.

## 3.1.0.0

*   Fix up deprecated functions used in Setup.hs.  This should allow Termonad to
    be compiled with Cabal-3.0.0.0 (which is used by default in GHC-8.8).
    [#144](https://github.com/cdepillabout/termonad/pull/144) Thanks
    [mdorman](https://github.com/mdorman)!

*   Fully update to LTS-15 and GHC-8.8.  Termonad now requires GHC-8.8 in order
    to be compiled. [#145](https://github.com/cdepillabout/termonad/pull/145).

## 3.0.0.0

*   Remove the one-pixel white border around the `GtkNotebook` (the GTK widget thing
    that contains the tabs). [#138](https://github.com/cdepillabout/termonad/pull/138)

*   Add a right-click menu for the terminal.  It currently allows copy and
    paste.  [#136](https://github.com/cdepillabout/termonad/pull/136)  Thanks
    @jecaro!

*   Add a preferences file that settings will be saved to and read from at
    `~/.config/termonad/termonad.yaml`.  You can change settings with the
    Preferences dialog.  **The settings will only be used from this file if you
    do not have a `~/.config/termonad/termonad.hs` file**.
    [#140](https://github.com/cdepillabout/termonad/pull/140) Thanks again
    @jecaro!

## 2.1.0.0

*   Add a menu option to set preferences for a running Termonad session.
    The preferences you have set are lost when you end the Termonad session.
    [#130](https://github.com/cdepillabout/termonad/pull/130)  Thanks @jecaro!

## 2.0.0.0

*   Added menu option to search for a regex within the terminal output.
    This removes support for versions of VTE-2.91 older than 0.46.
    This means that compiling on older versions of Debian and Ubuntu may no
    longer work. [#118](https://github.com/cdepillabout/termonad/pull/118)

## 1.3.0.0

*   Change all uses of
    [`Colour`](http://hackage.haskell.org/package/colour-2.3.5/docs/Data-Colour.html#t:Colour)
    to
    [`AlphaColour`](http://hackage.haskell.org/package/colour-2.3.5/docs/Data-Colour.html#t:AlphaColour)
    in `Termonad.Config.Colour`.  Users should now use `AlphaColour` instead of
    `Colour`.  Also, all uses of `sRGB24` should be replaced with `createColour`.
    This change is mechanical and should not affect how Termonad works at all.
    Thanks to @jecaro and @amir! [#116](https://github.com/cdepillabout/termonad/pull/116)

## 1.2.0.0

*   Got the code for setting the backgroud color of the terminal actually
    working.  Thanks @dakotaclemenceplaza.
    [#111](https://github.com/cdepillabout/termonad/pull/111)
    *   This changes the type of `ColourConfig` to make the foreground and
        background colors of the terminal optional.

*   Various cleanup in the nix files.

## 1.1.0.0

*   Added an
    [example](https://github.com/cdepillabout/termonad/blob/0cd741d51958806092418b55abdf1c1dc078841c/example-config/ExampleSolarizedColourExtension.hs)
    of how to setup a solarized color scheme. Thanks @craigem.
    [#90](https://github.com/cdepillabout/termonad/pull/90) and [#103](https://github.com/cdepillabout/termonad/pull/103)

*   Various fixes in the nix files.  Make sure Termonad can see the GTK icons.
    [#91](https://github.com/cdepillabout/termonad/pull/91) and
    [#92](https://github.com/cdepillabout/termonad/pull/92)

*   Add a menu option to change the font size at runtime.  You should be able to
    do this with the `Ctrl-+` and `Ctrl--` keys.
    [#95](https://github.com/cdepillabout/termonad/pull/95)

*   Get building with GHC 8.6. Thanks @clinty.
    [#98](https://github.com/cdepillabout/termonad/pull/98)

## 1.0.1.0

*   Stop using the `widgetSetFocusOnClick` function, which is not supported on
    older versions of GTK. This lets Termonad be compiled with older versions
    of GTK. [#87](https://github.com/cdepillabout/termonad/pull/87).

*   Add CI. [#87](https://github.com/cdepillabout/termonad/pull/87).

*   Support versions of VTE-2.91 older than 0.44.
    [#87](https://github.com/cdepillabout/termonad/pull/87).

*   Add some functions for converting from a list to a `Vec` in
    `Termonad.Config.Vec`: `fromListVec` and `fromListVec_`.  Commit 883eb98b5f.

*   Fix the paste hotkey. [#86](https://github.com/cdepillabout/termonad/pull/86).

## 1.0.0.0

*   The API for configuring Termonad is now completely different. Many, many
    changes have gone into this version.  You should approach it as a
    completely different application.

    The CHANGELOG will be kept up-to-date for future releases.

## 0.2.1.0

*   Make sure the window title is set to "Termonad".

*   Relabel tabs when termonad is started.

## 0.2.0.0

*   Open dialog asking if you want to quit when you try to use your WM to quit.

*   Termonad will attempt to open up a new terminal in the working directory of
    the current terminal.

*   Make sure termonad won't crash if dyre can't find GHC.

*   Add a few more ways to compile on NixOS.

*   Add an icon for termonad.

## 0.1.0.0

*   Initial release.
