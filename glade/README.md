
# Termonad Glade Files

This directory contains [Glade](https://wiki.gnome.org/Apps/Glade) files used
by Termonad to define how the GTK interfaces should look.

Glade is an visual interface designer.  The Glade files are just XML files.
They can be edited in the Glade program, or directly by hand.

If you're new to Glade, you may be interested in going through a
[tutorial](https://wiki.gnome.org/Apps/Glade/Tutorials).

If you use the [shell.nix](../shell.nix) file, it should pull in `glade` for
you, so you can just run it directly after entering into the Nix shell.  Or,
you can just install `glade` through your package-manager-of-choice.
