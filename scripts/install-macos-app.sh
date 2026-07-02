#!/usr/bin/env bash
#
# Create a Spotlight/Launchpad-discoverable "Termonad.app" in ~/Applications on
# macOS.
#
# WHY THIS IS NEEDED:
#
# When built through this repo's Nix setup, Termonad is a native macOS (Quartz)
# application, and `nix profile add` installs a proper app bundle at
# ~/.nix-profile/Applications/Termonad.app.  However, macOS Spotlight and
# Launchpad will NOT find that bundle: they don't index apps that live in the
# Nix store, and they refuse to index symlinked .app bundles (so simply
# symlinking it into ~/Applications does not work either).  See
# https://github.com/NixOS/nix/issues/7055 for more info.
#
# This script works around that by first assuming the user installed termonad
# with `nix profile add`, and then by dropping a tiny "trampoline" app into
# ~/Applications/Termonad.app.  It is a real (non-symlink) .app directory --
# which Spotlight does index -- containing:
#
#   * the installed bundle's Info.plist and icon (so the name and Dock icon
#     match), and
#   * a small launcher script that just re-execs the real binary at
#     ~/.nix-profile/bin/termonad.
#
# Because the launcher points at the stable ~/.nix-profile path (rather than a
# specific /nix/store/... path), this only has to be run ONCE: the trampoline
# keeps working across `nix profile upgrade`.  Re-run it only if you delete
# ~/Applications/Termonad.app or install Termonad into a different Nix profile.
#
# PREREQUISITE: install Termonad into your Nix profile first, e.g. from a
# checkout of this repo:
#
#   $ nix profile add .#termonad
#
# USAGE:
#
#   $ ./scripts/install-macos-app.sh
#
# To undo, just remove the trampoline:
#
#   $ rm -rf ~/Applications/Termonad.app
#
# NOTE: If you manage your Mac declaratively with nix-darwin or home-manager,
# prefer the `mac-app-util` module (https://github.com/hraban/mac-app-util),
# which does the same trampolining automatically on every rebuild.  This script
# is the low-tech, no-extra-dependencies alternative for a single app.

set -e

# Stable, upgrade-proof locations of the Nix-profile-installed Termonad.
profile_app="$HOME/.nix-profile/Applications/Termonad.app"
profile_bin="$HOME/.nix-profile/bin/termonad"

# Where the trampoline app is created.
dest="$HOME/Applications/Termonad.app"

if [ ! -e "$profile_bin" ]; then
  echo "error: $profile_bin not found." >&2
  echo "Install Termonad into your Nix profile first: nix profile add .#termonad" >&2
  exit 1
fi

mkdir -p "$HOME/Applications"
rm -rf "$dest"
mkdir -p "$dest/Contents/MacOS" "$dest/Contents/Resources"

# Reuse the installed bundle's metadata and icon so the name/icon stay in sync
# with whatever version is currently installed.  (Files copied out of the Nix
# store are read-only, so make them writable.)
cp "$profile_app/Contents/Info.plist" "$dest/Contents/Info.plist"
cp "$profile_app/Contents/Resources/termonad.icns" "$dest/Contents/Resources/termonad.icns"
chmod u+w "$dest/Contents/Info.plist" "$dest/Contents/Resources/termonad.icns"

# A real launcher script (not a symlink, so Spotlight indexes it) that execs the
# stable profile binary.  The path is baked in when this script runs.
cat > "$dest/Contents/MacOS/Termonad" <<EOF
#!/bin/sh
exec "$profile_bin"
EOF
chmod +x "$dest/Contents/MacOS/Termonad"

# Register with LaunchServices right away so it shows up in Spotlight without
# having to wait for the next indexing pass.
/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -f "$dest" || true

echo "Created $dest -> $profile_bin"
