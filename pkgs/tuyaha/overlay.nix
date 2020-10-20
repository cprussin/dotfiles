_: super: {
  python3 = super.python3.override {
    packageOverrides = pyself: _: {
      tuyaha = pyself.callPackage ./derivation.nix {};
    };
  };
}
