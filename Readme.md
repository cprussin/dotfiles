# Dotfiles

This repository contains all of my personal configurations.  You can use my
configurations!

Probably, you'll want to extract stuff out of my configuration for your own use.
The way things are structured here is:

- [config](./config): This folder contains all configurations.  It is organized
  into the following subdirectories:
  - [machines/\<name>](./config/machines): These folders contain
    machine-specific configurations.  Each folder should contain a
    `default.nix`, which is the entry point to the nixos configuration (this
    file is used as `/etc/nixos/configuration.nix`).
  - [modules](./config/modules): The meat of the configuration, this directory
    contains all the different configs for the OS, services, and applications.
    For the most part, modules in here are not actually loaded directly but
    instead collected into profiles, which are consumed by machine
    configurations (see below).
  - [profiles](./config/profiles): This directory contains collections of
    modules packaged into common configuration sets.  For the most part, machine
    configs import profiles, which then import modules.
- [lib](./lib): This folder contains various utility functions that are used
  throughout the codebase.
- [modules](./modules): This folder contains custom abstract configuration
  modules.  They provide configuration for programs and systems that are missing
  in nixos or home-manager.  In some cases, things in here are things I'd
  eventually like to polish off and send as pull requests to nixos or
  home-manager.  In other cases they are experimental ideas that likely don't
  actually belong upstream, like [configuring the whole system's color theme in
  one place](./modules/home-manager/color-theme.nix).
- [overlays](./overlays): This directory contains nixpkgs overlays.
- [pkgs](./pkgs): This directory contains custom packages.

If you wanted to use my configurations directly, you would need to do the
following:

1. Create a new [machine configuration](./config/machines).

2. Symlink your machine configuration to to `./current-machine`:

```bash
ln -s config/machines/<my machine> current-machine
```

3. Remove your nix channels (This isn't strictly necessary, but I don't use
   channels and instead I manage my nixpkgs version using niv, see
   [shell.nix](./shell.nix) and
   [nix-path/default.nix](./config/modules/nix/nix-path/default.nix)):

```bash
nix-channel remove nixpkgs
```

4. Deploy it!

```bash
nix-shell --run deploy
```
