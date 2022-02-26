_: super: {
  python3 = super.python3.override {
    packageOverrides = pyself: _: {
      hacs-frontend = pyself.callPackage ./derivation.nix {};
    };
  };
}
