
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

<!-- markdown-toc start - Run M-x markdown-toc-generate-toc again or edit manually. -->
**Table of Contents**
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
$ pacman -S 
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

## Maintainers

- [Dennis Gosnell](https://github.com/cdepillabout)
