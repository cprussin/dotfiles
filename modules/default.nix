{ ... }:

{
  imports = [
    ./nixos/crypt-initrd.nix
    ./nixos/primary-user.nix
    ./nixos/secure.nix
    ./nixos/sudo-cmds.nix
  ];

  primary-user.home-manager = _: {
    imports = [
      ./home-manager/cursor-theme.nix
      ./home-manager/default-terminal.nix
      ./home-manager/font.nix
      ./home-manager/icon-theme.nix
      ./home-manager/mako.nix
      ./home-manager/swaylock.nix
      ./home-manager/waybar.nix
    ];
  };
}
