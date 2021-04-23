{ ... }:

{
  imports = [
    ./nixos/autologin-graphical-session.nix
    ./nixos/color-theme.nix
    ./nixos/detachedLuksWithNixopsKeys.nix
    ./nixos/keymap.nix
    ./nixos/network-interfaces.nix
    ./nixos/powerpanel.nix
    ./nixos/preLVMTempMount.nix
    ./nixos/primary-user.nix
    ./nixos/route53DynamicDns.nix
    ./nixos/secure.nix
    ./nixos/sudo-cmds.nix
    ./nixos/umask.nix
  ];

  primary-user.home-manager = _: {
    imports = [
      ./home-manager/color-theme.nix
      ./home-manager/cursor-theme.nix
      ./home-manager/default-terminal.nix
      ./home-manager/emacs-rc.nix
      ./home-manager/font.nix
      ./home-manager/fzf.nix
      ./home-manager/icon-theme.nix
      ./home-manager/imv.nix
      ./home-manager/keymap.nix
      ./home-manager/launcher.nix
      ./home-manager/mako.nix
      ./home-manager/swayidle.nix
      ./home-manager/swaylock.nix
      ./home-manager/waybar.nix
    ];
  };
}
