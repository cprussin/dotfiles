{ config, ... }:

{
  nixpkgs = {
    config = import ./nixpkgs-config.nix;
    overlays = [
      (import ../../../overlays/dircolors-solarized.nix)
      (import ../../../overlays/emacs-with-packages.nix)
      (import ../../../overlays/get-aws-access-key.nix)
      (import ../../../overlays/gnupg.nix)
      (import ../../../overlays/launcher.nix)
      (import ../../../overlays/mako.nix)
      (import ../../../overlays/notify-send.nix)
      (import ../../../overlays/pass-with-otp.nix)
      (import ../../../overlays/slack.nix)
      (import ../../../overlays/sudo-with-insults.nix)
      (import ../../../overlays/waybar-with-pulse.nix)
      (import ../../../overlays/zoom-frm.nix)
      (import ../../../overlays/zsh-git-prompt.nix)
    ];
  };

  home-manager.users.${config.primaryUserName} = {
    nixpkgs = {
      config = config.nixpkgs.config;
      overlays = config.nixpkgs.overlays;
    };

    xdg.configFile."nixpkgs/config.nix".source = ./nixpkgs-config.nix;
  };
}
