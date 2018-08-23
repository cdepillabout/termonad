
Termonad
=========

[![Build Status](https://secure.travis-ci.org/cdepillabout/termonad.svg)](http://travis-ci.org/cdepillabout/termonad)
[![Hackage](https://img.shields.io/hackage/v/termonad.svg)](https://hackage.haskell.org/package/termonad)
[![Stackage LTS](http://stackage.org/package/termonad/badge/lts)](http://stackage.org/lts/package/termonad)
[![Stackage Nightly](http://stackage.org/package/termonad/badge/nightly)](http://stackage.org/nightly/package/termonad)
![BSD3 license](https://img.shields.io/badge/license-BSD3-blue.svg)

Termonad is a terminal emulator configurable in Haskell.  It is extremely
customizable and provides hooks to modify the default behavior.  It can be
thought of as the "XMonad" of terminal emulators.

![image of Termonad](./img/termonad.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Termonad](#termonad)
    - [Installation](#installation)
        - [Arch Linux](#arch-linux)
        - [Ubuntu / Debian](#ubuntu--debian)
        - [NixOS](#nixos)
    - [How to use Termonad](#how-to-use-termonad)
    - [Goals](#goals)
    - [Maintainers](#maintainers)

<!-- markdown-toc end -->

## Installation

Termonad can be able to be installed on any system as long as the necessary GTK
libraries are installed.  The following lists instructions for a few
distributions.  If the given steps don't work for you, or you want to add
instructions for an additional distribution, please send a pull request.

The following steps use the
[`stack`](https://docs.haskellstack.org/en/stable/README/) build tool to build
Termonad, but [`cabal`](https://www.haskell.org/cabal/) can be used as well.
If `cabal` doesn't work for you, please open an issue or send a pull request.

### Arch Linux

First, you must install the required GTK system libraries:

```sh
$ pacman -S vte3
```

You must have `stack` to be able to build Termonad.  Steps for
installing `stack` can be found on
[this page](https://docs.haskellstack.org/en/stable/install_and_upgrade/).

In order to install Termonad, clone this repository and run `stack install`.
This will install the `termonad` binary to `~/.local/bin/`:

```sh
$ git clone https://github.com/cdepillabout/termonad
$ cd termonad/
$ stack install
```

### Ubuntu / Debian

First, you must install the required GTK system libraries:

```sh
$ apt-get install gobject-introspection libgirepository1.0-dev libgtk-3-dev libvte-2.91-dev
```

You must have `stack` to be able to build Termonad.  Steps for
installing `stack` can be found on
[this page](https://docs.haskellstack.org/en/stable/install_and_upgrade/).

In order to install Termonad, clone this repository and run `stack install`.
This will install the `termonad` binary to `~/.local/bin/`:

```sh
$ git clone https://github.com/cdepillabout/termonad
$ cd termonad/
$ stack install
```

### NixOS

There are two methods to build Termonad on NixOS.

The first is using `stack`.  The following commands install `stack` for your
user, clone this repository, and install the `termonad` binary to `~/.local/bin/`:

```sh
$ nix-env -i stack
$ git clone https://github.com/cdepillabout/termonad
$ cd termonad/
$ stack --nix install
```

The second is using the normal `nix-build` machinery.  The following commands
clone this repository and build the `termonad` binary at `./result/bin/`:

```sh
$ git clone https://github.com/cdepillabout/termonad
$ cd termonad/
$ nix-build
```

## How to use Termonad

Termonad is similar to XMonad. The above steps will install a `termonad` binary
somewhere on your system. If you have installed Termonad using `stack`, the
`termonad` binary will be in `~/.local/bin/`. This binary is a version of
Termonad configured with default settings. You can try running it to get an idea
of what Termonad is like:

```sh
$ ~/.local/bin/termonad
```

If you would like to configure termonad with your own settings, first you will
need to create a Haskell file called `~/.config/termonad/termonad.hs`. The
next section gives an example configuration file.

If this file exists, when the `~/.local/bin/termonad` binary launches, it will
try to compile it. If it succeeds, it will create a separate binary file called
something like `~/.cache/termonad/termonad-linux-x86_64`. This binary file can
be thought of as your own personal Termonad, configured with all your own
settings.

When you run `~/.local/bin/termonad`, it will re-exec
`~/.cache/termonad/termonad-linux-x86_64` if it exists.

However, there is one difficulty with this setup. In order for the
`~/.local/bin/termonad` binary to be able to compile your
`~/.config/termonad/termonad.hs` file, it needs to know where GHC is, as well as
where all your Haskell packages live. This presents some difficulties that will
be discussed in a following section.

### Configuring Termonad

The following is an example Termonad configuration file. You should save this to
`~/.config/termonad/termonad.hs`. You can find more information on the available
configuration options within the
[`Termonad.Config`](https://hackage.haskell.org/package/termonad/docs/Termonad-Config.html)
module.

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Colour.SRGB (Colour, sRGB24)
import Termonad.App (defaultMain)
import Termonad.Config
  ( FontConfig, ShowScrollbar(ShowScrollbarAlways), cursorColor
  , defaultFontConfig, defaultTMConfig, fontConfig, fontFamily
  , fontSize, showScrollbar
  )

-- | This sets the color of the cursor in the terminal.
--
-- This uses the "Data.Colour" module to define a dark-red color.
-- There are many default colors defined in "Data.Colour.Names".
cursColor :: Colour Double
cursColor = sRGB24 204 0 0

-- | This defines the font for the terminal.
fontConf :: FontConfig
fontConf =
  defaultFontConfig
    { fontFamily = "DejaVu Sans Mono"
    , fontSize = 13
    }

main :: IO ()
main = do
  let termonadConf =
        defaultTMConfig
          { cursorColor = cursColor
          , fontConfig = fontConf
          , showScrollbar = ShowScrollbarAlways
          }
  defaultMain termonadConf
```

## Goals

## Maintainers

- [Dennis Gosnell](https://github.com/cdepillabout)
