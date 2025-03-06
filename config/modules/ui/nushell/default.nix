{pkgs, ...}: {
  home-manager.users.root.imports = [./home.nix];

  nixpkgs.overlays = [
    (_: super: {
      nushell = super.nushell.override {
        additionalFeatures = base: base ++ ["sqlite"];
      };
    })
  ];

  primary-user = {
    home-manager.imports = [./home.nix];
    shell = "${pkgs.nushell}/bin/nu";
  };
}
