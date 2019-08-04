{ pkgs, ... }:

{
  nixpkgs.overlays = [
    (self: super: {
      lock-screen = super.callPackage ./lock-screen.nix {};
    })
  ];

  services.screen-locker = {
    enable = true;
    lockCmd = "${pkgs.lock-screen}/bin/lock-screen";
  };
}
