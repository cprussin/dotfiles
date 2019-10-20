{ pkgs, config, ... }:

{
  nixpkgs.overlays = [
    (self: super: {
      volume = super.callPackage ./volume.nix {};
    })
  ];

  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
    support32Bit = true;
  };

  users.users.${config.primaryUserName}.extraGroups = [ "audio" ];
}
