{ ... }:

{
  imports = [
    ./nixos/autologin-graphical-session.nix
    ./nixos/color-theme.nix
    ./nixos/keymap.nix
    ./nixos/luks-external-key-devices.nix
    ./nixos/primary-user.nix
    ./nixos/secure.nix
    ./nixos/sudo-cmds.nix
    ./nixos/wifi-interface.nix
  ];

  primary-user.home-manager = _: {
    imports = [
      ./home-manager/color-theme.nix
      ./home-manager/cursor-theme.nix
      ./home-manager/default-terminal.nix
      ./home-manager/font.nix
      ./home-manager/fzf.nix
      ./home-manager/icon-theme.nix
      ./home-manager/keymap.nix
      ./home-manager/swaylock.nix
      ./home-manager/waybar.nix
    ];
  };
}
