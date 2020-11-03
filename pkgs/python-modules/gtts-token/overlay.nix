_: super: {
  python3 = super.python3.override {
    packageOverrides = pyself: _: {
      gtts-token = pyself.callPackage ./derivation.nix { };
    };
  };
}
