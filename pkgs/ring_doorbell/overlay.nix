_: super: {
  python3 = super.python3.override {
    packageOverrides = pyself: _: {
      ring_doorbell = pyself.callPackage ./derivation.nix {};
    };
  };
}
