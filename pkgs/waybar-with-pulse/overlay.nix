self: super: {
  waybar = self.callPackage ./derivation.nix {
    waybar = super.waybar;
  };
}
