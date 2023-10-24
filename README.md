
Termonad
=========

[![CI](https://github.com/cdepillabout/termonad/actions/workflows/ci.yml/badge.svg)](https://github.com/cdepillabout/termonad/actions/workflows/ci.yml)
[![Hackage](https://img.shields.io/hackage/v/termonad.svg)](https://hackage.haskell.org/package/termonad)
[![Stackage LTS](http://stackage.org/package/termonad/badge/lts)](http://stackage.org/lts/package/termonad)
[![Stackage Nightly](http://stackage.org/package/termonad/badge/nightly)](http://stackage.org/nightly/package/termonad)
[![BSD3 license](https://img.shields.io/badge/license-BSD3-blue.svg)](./LICENSE)
[![Join the chat at https://gitter.im/termonad/Lobby](https://badges.gitter.im/termonad/Lobby.svg)](https://gitter.im/termonad/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Termonad is a terminal emulator configurable in Haskell.  It is extremely
customizable and provides hooks to modify the default behavior.  It can be
thought of as the "XMonad" of terminal emulators.

![image of Termonad](./img/termonad.png)

Termonad was
[featured on an episode](https://www.youtube.com/watch?v=TLNr_gBv5HY) of
[DistroTube](https://www.youtube.com/channel/UCVls1GmFKf6WlTraIb_IaJg).
This video gives a short overview of Termonad.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Termonad](#termonad)
  - [Installation](#installation)
    - [Arch Linux](#arch-linux)
    - [Ubuntu / Debian](#ubuntu--debian)
    - [Nix](#nix)
    - [Mac OS X](#mac-os-x)
      - [Installing with just `stack`](#installing-with-just-stack)
      - [Installing with just `nix`](#installing-with-just-nix)
      - [Installing with `stack` using `nix`](#installing-with-stack-using-nix)
    - [Windows](#windows)
  - [How to use Termonad](#how-to-use-termonad)
    - [Default Key Bindings](#default-key-bindings)
    - [Configuring Termonad](#configuring-termonad)
    - [Compiling Local Settings](#compiling-local-settings)
      - [Running with `stack`](#running-with-stack)
      - [Running with `nix`](#running-with-nix)
  - [Goals](#goals)
  - [Where to get help](#where-to-get-help)
  - [Contributions](#contributions)
  - [Maintainers](#maintainers)

<!-- markdown-toc end -->

## Installation

Termonad can be installed on any system as long as the necessary GTK libraries
are available.  The following are instructions for installing Termonad on a few
different distributions and systems.  If the given steps don't work for you, or
you want to add instructions for an additional system, please send a pull
request.

The following steps use the
[`stack`](https://docs.haskellstack.org/en/stable/README/) build tool to build
Termonad, but [`cabal`](https://www.haskell.org/cabal/) can be used as well. Steps for
installing `stack` can be found on
[this page](https://docs.haskellstack.org/en/stable/install_and_upgrade/).


### Arch Linux

First, you must install the required Gnome/GTK system libraries:

```sh
$ pacman -S vte3 gobject-introspection
```

In order to install Termonad, clone this repository and run `stack install`.
This will install the `termonad` binary to `~/.local/bin/`:

```sh
$ git clone https://github.com/cdepillabout/termonad
$ cd termonad/
$ stack install
```

Note that Termonad depends on the [`haskell-gi`](https://github.com/haskell-gi/haskell-gi) family of Haskell libraries.  `haskell-gi` contains Haskell wrappers for for Gnome/GTK system libraries.  It uses the [GObject Introspection](https://gi.readthedocs.io/en/latest/) functionality from the Gnome libraries.

One problem that Arch users often run into is that their system Gnome/GTK libraries are newer than what the `haskell-gi` dependencies from Stackage support.  If you run into this problem, there are a couple things you can try:

- Manually switch to a newer Stackage resolver (probably Stackage Nightly).  Newer Stackage resolvers often have newer versions of the `haskell-gi` libraries.  Newer versions of the `haskell-gi` libraries are more likely to support your newer system Gnome/GTK libraries.  If you get something working like this, please open a PR.
- Use `cabal` for building Termonad instead of `stack`.  Make sure `cabal`'s constraint solver picks the latest versions of the `haskell-gi` libraries on Hackage.
- Use Nix for installing Termonad.

My suggestion is to use Nix, since it is highly likely to "just work" (because with Nix, _all_ libraries are pinned to known working versions, even system libraries).

### Ubuntu / Debian

Termonad can be installed through `apt` on Debian and Ubuntu:

```console
$ sudo apt install termonad libghc-termonad-dev
```

Note that the `libghc-termonad-dev` package is necessary if you want to be able
to compile the Haskell-based settings file, `termonad.hs`.

#### Compiling from source on Ubuntu / Debian

First, you must install the required Gnome/GTK system libraries:

```sh
$ apt-get install gobject-introspection libgirepository1.0-dev libgtk-3-dev libvte-2.91-dev libpcre2-dev
```

In order to install Termonad, clone this repository and run `stack install`.
This will install the `termonad` binary to `~/.local/bin/`:

```sh
$ git clone https://github.com/cdepillabout/termonad
$ cd termonad/
$ stack install
```

### Nix

If you have `nix` installed, you should be able to use it to obtain Termonad.
This means that it will work on NixOS, or with `nix` on another distro.  There
are three different ways to use `nix` to get Termonad:

1.  Get Termonad from Nixpkgs.  Termonad is provided as a top-level `termonad`
    attribute in Nixpkgs.

    For instance, run a `nix-shell` with Termonad:

    ```console
    $ nix-shell -p termonad
    $ termonad   # run termonad within the nix-shell
    ```

    You can also install `termonad` with tools like `nix-env` or home-manager.
    If you're using NixOS, you can add `termonad` to your
    `environment.systemPackages` list.

    Keep in mind that if you're using an old release of NixOS, you'll likely
    get an older version of Termonad.

2.  Build Termonad using the code in this repository. The following commands
    clone this repo and build the `termonad` binary at `./result/bin/`:

    ```sh
    $ git clone https://github.com/cdepillabout/termonad
    $ cd termonad/
    $ nix-build
    ```

3.  Build Termonad using `stack` with Nix-integration. The following commands
    install `stack` for your user, clone this repository, and install the
    `termonad` binary to `~/.local/bin/`:

    ```sh
    $ nix-env -i stack
    $ git clone https://github.com/cdepillabout/termonad
    $ cd termonad/
    $ stack --nix install
    ```

    (_edit_: Building with `stack` using Nix-integration does not currently work.
    See [#99](https://github.com/cdepillabout/termonad/issues/99).)

### Mac OS X

Building and installing Termonad on Mac OS X should be possible with any of the following three methods:

-   Install the required system libraries (like GTK and VTE) by hand, then use
    `stack` to build Termonad.

    This is probably the easiest method.  You don't have to understand anything
    about `nix`.  However, it is slightly annoying to have to install GTK and
    VTE by hand.

-   Use `nix` to install both the required system libraries and Termonad itself.

    If you are a nix user and want an easy way to install Termonad, this
    is the recommended method.

-   Use `nix` to install install the required system libraries, and `stack` to
    build Termonad.

    If you are a nix user, but want to use `stack` to actually do development
    on Termonad, using `stack` may be easier than using `cabal`.

The following sections describe each method.

#### Installing with just `stack`

(*currently no instructions available.  please send a PR adding instructions if you get termonad to build using this method.*)

#### Installing with just `nix`

`nix` can be used to install Termonad with the following steps, assuming you
have `nix` [installed](https://nixos.org/nix/download.html).  These commands
clone this repository and build the `termonad` binary at `./result/bin/`:

```sh
$ git clone https://github.com/cdepillabout/termonad
$ cd termonad/
$ nix-build
```

#### Installing with `stack` using `nix`

`stack` can be used in conjunction with `nix` to install Termonad.  `nix` will
handle installing system dependencies (like GTK and VTE), while `stack` will
handle compiling and installing Haskell packages.

You must have `nix` [installed](https://nixos.org/nix/download.html).

You will also need `stack` installed.  You can do that with the following command:

```sh
$ nix-env -i stack
```

After `stack` is installed, you will need to clone Termonad and build it:

```
$ git clone https://github.com/cdepillabout/termonad
$ cd termonad/
$ stack --nix install
```

This will install the `termonad` binary to `~/.local/bin/`.

### Windows

To run Termonad on Windows, you'll need:

* any X server app, for example **Vcxsrv**
* any WSL, for example **Ubuntu**

I'm using both Vcxsrv and Ubuntu WSL.

Configure both Vcxsrv and WSL. For Vcxsrv go with default settings
everywhere, it will be fine. Configure your WSL as you want (choose
your name etc.). After you set up the user, you'll have to update your
OS, run:

```console
$ sudo apt-get update
$ sudo apt-get upgrade -y
$ sudo apt-get dist-upgrade -y
$ sudo apt-get autoremove -y
```

Configure the `DISPLAY` environment variable for the X server, and load the changes in bash:

For WSL1:

```console
$ echo "export DISPLAY=localhost:0.0" >> ~/.bashrc
$ source ~/.bashrc
```

For WSL2:

```console
$ echo export DISPLAY=$(awk '/nameserver / {print $2; exit}' /etc/resolv.conf 2>/dev/null):0 >> ~/.bashrc
$ echo export LIBGL_ALWAYS_INDIRECT=1 >> ~/.bashrc
$ source ~/.bashrc
```

If you're using WSL2, you have to create a separate **inbound rule** for TCP port 6000, to allow WSL access to the X server.
If you're using mentioned earlier **Vcxsrv** you can enable public access for your X server by disabling Access Control on the Extra Settings.
You can also use `-ac` flag in the Additional parameters for VcXsrv section.

Your X server should now be configured.

Execute following command to install the necessary GTK system libraries:

```console
$ apt-get install gobject-introspection libgirepository1.0-dev libgtk-3-dev libvte-2.91-dev libpcre2-dev
```

The required GTK system libraries should now be installed.

Clone the Termonad repo:

```sh
$ git clone https://github.com/cdepillabout/termonad
$ cd termonad/
$ stack build
$ stack run
```
After `stack run`, you should see a new window with your Termonad running.

## How to use Termonad

Termonad is similar to XMonad. The above steps will install a `termonad` binary
somewhere on your system. If you have installed Termonad using `stack`, the
`termonad` binary will be in `~/.local/bin/`. If you have installed Termonad using
your Linux distro, the `termonad` binary will likely be in `/usr/bin/`. This
binary is a version of Termonad configured with default settings. You can try
running it to get an idea of what Termonad is like:

```sh
$ /usr/bin/termonad
```

If you would like to configure Termonad with your own settings, first you will
need to create a Haskell file called `~/.config/termonad/termonad.hs`. A following
section gives an example configuration file.

If this configuration file exists, when the `/usr/bin/termonad` binary
launches, it will try to use GHC to compile the configuration file. If GHC
is able to successfully compile the configuration file, a separate binary will
be created called something like `~/.cache/termonad/termonad-linux-x86_64`.
This binary file can be thought of as your own personal Termonad, configured
with all your own settings.

When you run `/usr/bin/termonad`, it will re-exec
`~/.cache/termonad/termonad-linux-x86_64` if it exists.

However, there is one difficulty with this setup. In order for the
`/usr/bin/termonad` binary to be able to compile your
`~/.config/termonad/termonad.hs` configuration file, Termonad needs to know
where GHC is, as well as where all your Haskell packages live. This presents
some difficulties that will be discussed in one of the following sections.

### Default Key Bindings

Termonad provides the following default key bindings.

| Key binding | Action |
|------------|--------|
| <kbd>Ctrl</kbd> <kbd>Shift</kbd> <kbd>t</kbd> | Open new tab. |
| <kbd>Ctrl</kbd> <kbd>Shift</kbd> <kbd>w</kbd> | Close tab. |
| <kbd>Ctrl</kbd> <kbd>Shift</kbd> <kbd>f</kbd> | Open Find dialog for searching for a regex. |
| <kbd>Ctrl</kbd> <kbd>Shift</kbd> <kbd>p</kbd> | Find the regex **above** the current position. |
| <kbd>Ctrl</kbd> <kbd>Shift</kbd> <kbd>i</kbd> | Find the regex **below** the current position. |
| <kbd>Ctrl</kbd> <kbd>+</kbd> | Increase font size. |
| <kbd>Ctrl</kbd> <kbd>-</kbd> | Decrease font size. |
| <kbd>Ctrl</kbd> <kbd>PgUp</kbd> | Switch to previous tab. |
| <kbd>Ctrl</kbd> <kbd>PgDown</kbd> | Switch to next tab. |
| <kbd>Alt</kbd> <kbd>(number key)</kbd> | Switch to tab `number`.  For example, <kbd>Alt</kbd> <kbd>2</kbd> switches to tab 2. |

### Configuring Termonad

Termonad has three different ways to be configured.

1.  Pass arguments on the command line.  For instance, run
    `termonad --no-show-menu` to never show the `File` menubar.

    Arguments passed on the command line will normally override other
    configuration methods.

2.  Use the built-in Preferences editor.  You can find this in
    the `Preferences` menu under `Edit` in the menubar.

    When opening Termonad for the first time, it will create a preferences file
    at `~/.config/termonad/termonad.yaml`.  When you change a setting in the
    Preferences editor, Termonad will update the setting in the preferences
    file.

    When running Termonad, it will load settings from the preferences file. Do
    not edit the preferences file by hand, because it will be overwritten when
    updating settings in the Preferences editor.

    This method is perfect for users who only want to make small changes to the
    Termonad settings, like the default font size.

3.  Use a Haskell-based settings file, called `~/.config/termonad/termonad.hs` by default.
    This method allows you to make large, sweeping changes to Termonad.  This
    method is recommended for power users.

    The rest of this section explains the `~/.config/termonad/termonad.hs` file.

**WARNING: If you have a `~/.config/termonad/termonad.hs` file, then all
settings from `~/.config/termonad/termonad.yaml` will be ignored.  If you want
to set *ANY* settings in `~/.config/termonad/termonad.hs`, then you must
set *ALL* settings in `~/.config/termonad/termonad.hs`.  However, as stated above,
CLI arguments will override settings in `~/.config/termonad/termonad.hs` by default.**

The following is an example Termonad configuration file. You should save this to
`~/.config/termonad/termonad.hs`. You can find more information on the available
configuration options within the
[`Termonad.Config`](https://hackage.haskell.org/package/termonad/docs/Termonad-Config.html)
module.

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Termonad
  ( FontConfig, FontSize(FontSizePoints), Option(Set)
  , ShowScrollbar(ShowScrollbarAlways), defaultConfigOptions, defaultFontConfig
  , defaultMain, defaultTMConfig, fontConfig, fontFamily, fontSize, options
  , showScrollbar
  )
import Termonad.Config.Colour
  ( AlphaColour, ColourConfig, addColourExtension, createColour
  , createColourExtension, cursorBgColour, defaultColourConfig
  )

-- | This sets the color of the cursor in the terminal.
--
-- This uses the "Data.Colour" module to define a dark-red color.
-- There are many default colors defined in "Data.Colour.Names".
cursBgColour :: AlphaColour Double
cursBgColour = createColour 204 0 0

-- | This sets the colors used for the terminal.  We only specify the background
-- color of the cursor.
colConf :: ColourConfig (AlphaColour Double)
colConf =
  defaultColourConfig
    { cursorBgColour = Set cursBgColour
    }

-- | This defines the font for the terminal.
fontConf :: FontConfig
fontConf =
  defaultFontConfig
    { fontFamily = "DejaVu Sans Mono"
    , fontSize = FontSizePoints 13
    }

main :: IO ()
main = do
  colExt <- createColourExtension colConf
  let termonadConf =
        defaultTMConfig
          { options =
              defaultConfigOptions
                { fontConfig = fontConf
                  -- Make sure the scrollbar is always visible.
                , showScrollbar = ShowScrollbarAlways
                }
          }
        `addColourExtension` colExt
  defaultMain termonadConf
```

There are other example configuration files in the
[example-config/](./example-config) directory.

If you want to test what all the colors look like, you may find it convenient
to use the
[`print-console-colors`](http://hackage.haskell.org/package/print-console-colors)
package, which provides an executable called `print-console-colors` that prints
all of the colors for your terminal.

### Compiling Local Settings

If you launch Termonad by calling `/usr/bin/termonad`, it will try to
compile the `~/.config/termonad/termonad.hs` file if it exists.  The problem is
that `/usr/bin/termonad` needs to be able to see GHC and the required
Haskell libraries to be able to compile `~/.config/termonad/termonad.hs`.

There are a couple solutions to this problem, listed in the sections below.

(These steps are definitely confusing. I would love to figure out a better
way to do this.  Please submit an issue or PR if you have a good idea about
how to fix this.)

#### Running with `stack`

If you originally compiled Termonad with `stack` and installed it to
`~/.local/bin/termonad`, you can use `stack` to execute Termonad.  First, you
must change to the directory with the Termonad source code.  From there, you
can run `stack exec`:

```sh
$ cd termonad/  # change to the termonad source code directory
$ stack exec -- termonad
```

`stack` will pick up the correct GHC version and libraries from the
`stack.yaml` and `termonad.cabal` file.  `termonad` will be run in an
environment with GHC available.  `termonad` will use this GHC and libraries to
compile your `~/.config/termonad/termonad.hs` file.  It if succeeds, it should
create a `~/.cache/termonad/termonad-linux-x86_64` binary.

If you need extra Haskell libraries available when compiling your
`~/.config/termonad/termonad.hs` file, you can specify them to `stack exec`:

```sh
$ stack exec --package lens --package conduit -- termonad
```

The problem with this is that `stack exec` changes quite a few of your
environment variables.  It is not recommended to actually run Termonad from
within `stack exec`.  After you run `stack exec -- termonad` and let it
recompile your `~/.config/termonad/termonad.hs` file, exit Termonad.
Re-run Termonad by calling it directly.  Termonad will notice that
`~/.config/termonad/termonad.hs` hasn't changed since
`~/.cache/termonad/termonad-linux-x86_64` has been recompiled, so it will
directly execute `~/.cache/termonad/termonad-linux-x86_64`.

#### Running with `nix`

Building Termonad with `nix` (by running `nix-build` in the top
directory) sets it up so that Termonad can see GHC.  Termonad should be able
to compile the `~/.config/termonad/termonad.hs` file by default.

If you're interested in how this works, or want to change which Haskell
packages are available from your `~/.config/termonad/termonad.hs` file, please
see the documentation in the
[`.nix-helpers/termonad-with-packages.nix`](./.nix-helpers/termonad-with-packages.nix)
file.

## Additional Info

This section contains some additional info that may be helpful for using Termonad.

### Opening URLs by right-clicking

It is possible to open a URL in a browser by right-clicking on it, and
selecting `Open URL in browser`.  In order for this you work, you may
have to setup your XDG defaults.  You can set the default browser to
Firefox with a command like the following:

```console
$ xdg-mime default firefox.desktop x-scheme-handler/http
$ xdg-mime default firefox.desktop x-scheme-handler/https
```

This `xdg-mime` executable comes from a package called `xdg-utils` in both
Nixpkgs and Ubutun/Debian.

## Goals

Termonad has the following goals:

* fully configurable in Haskell

  There are already
  [many](https://gnometerminator.blogspot.com/p/introduction.html)
  [good](https://www.enlightenment.org/about-terminology.md)
  [terminal](http://software.schmorp.de/pkg/rxvt-unicode.html)
  [emulators](https://launchpad.net/sakura).  However, there are no terminal
  emulators fully configurable in Haskell.  Termonad fills this niche.

* flexible

  Most people only need a terminal emulator that lets you change the font-size,
  cursor color, etc.  They don't need tons of configuration options.  Termonad
  should be for people that like lots of configuration options.  Termonad
  should provide many hooks to allow the user full control over its behavior.

* stable

  Termonad should be able to be used everyday as your main terminal
  emulator.  It should not crash for any reason.  If you experience a crash,
  please file an issue or a pull request!

* good documentation

  The [documentation](https://hackage.haskell.org/package/termonad) for
  Termonad on Hackage should be good.  You shouldn't have to guess at what
  certain data types or functions do.  If you have a hard time understanding
  anything in the documentation, please submit an issue or PR.

## Where to get help

If you find a bug in Termonad, please either
[send a PR](https://github.com/cdepillabout/termonad/pulls) fixing it or create
an [issue](https://github.com/cdepillabout/termonad/issues) explaining it.

If you just need help with configuring Termonad, you can either join the
[Gitter room](https://gitter.im/termonad/Lobby) or
[#termonad on irc.freenode.net](https://webchat.freenode.net/).

## Contributions

Contributions are highly appreciated.  Termonad is currently missing many
helpful configuration options and behavior hooks.  If there is something you
would like to add, please submit an issue or PR.

## Maintainers

- [cdepillabout](https://github.com/cdepillabout)
