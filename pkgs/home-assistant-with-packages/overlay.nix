self: super: {
  home-assistant = self.callPackage ./derivation.nix {
    home-assistant = super.home-assistant;
  };
}
