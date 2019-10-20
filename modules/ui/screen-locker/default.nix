{ pkgs, config, ... }:

{
  nixpkgs.overlays = [
    (self: super: {
      lock-screen = super.callPackage ./lock-screen.nix {};
    })
  ];

  home-manager.users.${config.primaryUserName} = { ... }: {
    services.screen-locker = {
      enable = true;
      lockCmd = "${pkgs.lock-screen}/bin/lock-screen";
    };
  };
}
