# Dotfiles

This repository contains all of my personal configurations.  You can use my
configurations!

Probably, you'll want to extract stuff out of my configuration for your own use.
The way things are structured here is:

- [lib](./lib): This folder contains various utility functions that are used
  throughout the codebase.
- [machines/\<name>](./machines): This folder contains machine-specific
  configuration.  Each folder should contain:
  - `default.nix`: This is the entry point to the nixos configuration,
    equivalent to `/etc/nixos/configuration.nix`.
  - `hardware.nix`: This file contains machine-specific hardware configuration
    and is mostly generated while installing nixos.
- [modules](./modules): The meat of the configuration, this directory contains
  all the different configs for the OS, services, and applications.  For the
  most part, modules in here are not actually loaded directly but instead
  collected into profiles, which are consumed by machine configurations (see
  below).
- [overlays](./overlays): This directory contains nixpkgs overlays.
- [pkgs](./pkgs): This directory contains custom packages.
- [profiles](./profiles): This directory contains collections of modules
  packaged into common configuration sets.  For the most part, machine configs
  import profiles, which then import modules.

If you wanted to use my configurations directly, you would need to do the
following:

1. Create a new [machine configuration](./machines).

2. Symlink your machine configuration to to `./current-machine`:

```bash
ln -s machines/<my machine> current-machine
```

3. Remove your nix channels (This isn't strictly necessary, but I don't use
   channels and instead I manage my nixpkgs version using niv, see
   [shell.nix](./shell.nix) and
   [nix-path/default.nix](./modules/nix/nix-path/default.nix):

```bash
nix-channel remove nixpkgs
```

4. Deploy it!

```bash
nix-shell --run deploy
```
