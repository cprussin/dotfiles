_: super: {
  python3 = super.python3.override {
    packageOverrides = pyself: _: {
      aiogithubapi = pyself.callPackage ./derivation.nix {};
    };
  };
}
