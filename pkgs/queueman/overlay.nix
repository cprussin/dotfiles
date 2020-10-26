_: super: {
  python3 = super.python3.override {
    packageOverrides = pyself: _: {
      queueman = pyself.callPackage ./derivation.nix {};
    };
  };
}
