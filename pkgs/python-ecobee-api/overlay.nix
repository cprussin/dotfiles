_: super: {
  python3 = super.python3.override {
    packageOverrides = pyself: _: {
      python-ecobee-api = pyself.callPackage ./derivation.nix { };
    };
  };
}
