{ ... }:

{
  nixpkgs = {
    config = import ./config.nix;
    overlays = [
      (self: super: import ../../../pkgs { callPackage = super.callPackage; })
    ];
  };

  xdg.configFile = {
    "nixpkgs/config.nix".source = ./config.nix;
    "nixpkgs/overlays/localpkgs.nix".text = ''
      self: super: import ${../../../pkgs} { callPackage = super.callPackage; }
    '';
  };
}
