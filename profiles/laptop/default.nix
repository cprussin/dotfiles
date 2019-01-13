{ config, ... }:

let
  stateVersion = import ../../state-version.nix;
  home-manager = builtins.fetchTarball "https://github.com/rycee/home-manager/archive/release-${stateVersion}.tar.gz";
in

{
  imports = [
    ../../modules/boot/systemd-boot

    ../../modules/devices/bluetooth
    ../../modules/devices/printers
    ../../modules/devices/touchpad

    ../../modules/devtools/docker

    ../../modules/net/dhcp
    ../../modules/net/wifi

    ../../modules/netflix/hostname
    ../../modules/netflix/shakti-nginx
    ../../modules/netflix/vpn

    ../../modules/nix/auto-upgrade

    ../../modules/security/primary-user
    ../../modules/security/process-information-hiding
    ../../modules/security/sshd
    ../../modules/security/sudo

    ../../modules/ui/audio/system
    ../../modules/ui/dvp/system
    ../../modules/ui/fonts
    ../../modules/ui/greeting
    ../../modules/ui/log
    ../../modules/ui/opengl
    ../../modules/ui/setup-monitors/system
    ../../modules/ui/xorg

    ../../modules/zone/pacific

    "${home-manager}/nixos"
  ];

  home-manager.users.${config.primaryUserName} = { lib, pkgs, ... }: {
    imports = [
      ../../modules/data/email
      ../../modules/data/nogit
      ../../modules/data/unison
      ../../modules/data/xdg-user-dirs

      ../../modules/devtools/git

      ../../modules/netflix/metatron

      ../../modules/nix/nixpkgs

      ../../modules/security/gpg
      ../../modules/security/keepassxc
      ../../modules/security/ssh
      ../../modules/security/umask

      ../../modules/ui/audio/user
      ../../modules/ui/autocutsel
      ../../modules/ui/backgrounds
      ../../modules/ui/dvp/user
      ../../modules/ui/emacs
      ../../modules/ui/launcher
      ../../modules/ui/minichrome
      ../../modules/ui/numix-cursor-theme
      ../../modules/ui/readline
      ../../modules/ui/setup-monitors/user
      ../../modules/ui/solarized-theme
      ../../modules/ui/terminal
      ../../modules/ui/tray
      ../../modules/ui/urxvt
      ../../modules/ui/xmobar
      ../../modules/ui/xmonad
      ../../modules/ui/zsh
    ];

    home = {
      inherit stateVersion;

      # FIXME For some reason, if keepassxc isn't added to the environment, it won't
      # start from an absolute path.  The error is:
      #
      # qt.qpa.plugin: Could not find the Qt platform plugin "xcb" in ""
      # This application failed to start because no Qt platform plugin could be
      # initialized. Reinstalling the application may fix this problem.
      #
      # This should be fixed so keepassxc can be removed from the environment and
      # accessible via the launcher only.
      packages = lib.mkForce [ pkgs.keepassxc ];
    };
  };

  system = { inherit stateVersion; };
}
