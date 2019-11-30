{ config, ... }:

{
  imports = [
    ./nixos/crypt-initrd.nix
  ];

  home-manager.users.${config.primaryUserName} = _: {
    imports = [
      ./home-manager/mako.nix
      ./home-manager/swaylock.nix
      ./home-manager/waybar.nix
    ];
  };
}
