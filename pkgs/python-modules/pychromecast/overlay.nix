_: super: {
  python3 = super.python3.override {
    packageOverrides = pyself: _: {
      pychromecast = pyself.callPackage ./derivation.nix { };
    };
  };
}
