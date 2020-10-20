_: super: {
  python3 = super.python3.override {
    packageOverrides = pyself: _: {
      rachiopy = pyself.callPackage ./derivation.nix {};
    };
  };
}
