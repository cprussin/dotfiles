_: super: {
  python3 = super.python3.override {
    packageOverrides = pyself: _: {
      fnvhash = pyself.callPackage ./derivation.nix { };
    };
  };
}
