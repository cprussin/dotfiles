_: super: {
  python3 = super.python3.override {
    packageOverrides = pyself: _: {
      pysmartapp = pyself.callPackage ./derivation.nix { };
    };
  };
}
