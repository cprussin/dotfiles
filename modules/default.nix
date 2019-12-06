{ config, ... }:

{
  imports = [
    ./nixos/crypt-initrd.nix
    ./nixos/primary-user.nix
    ./nixos/secure.nix
    ./nixos/sudo-cmds.nix
  ];

  home-manager.users.${config.primary-user.name} = _: {
    imports = [
      ./home-manager/default-terminal.nix
      ./home-manager/icon-theme.nix
      ./home-manager/mako.nix
      ./home-manager/swaylock.nix
      ./home-manager/waybar.nix
    ];
  };
}
