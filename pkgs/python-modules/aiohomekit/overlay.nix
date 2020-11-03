_: super: {
  python3 = super.python3.override {
    packageOverrides = pyself: _: {
      aiohomekit = pyself.callPackage ./derivation.nix { };
    };
  };
}
