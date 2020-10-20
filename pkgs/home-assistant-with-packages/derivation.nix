{ home-assistant, callPackage }:

home-assistant.override {
  packageOverrides = pyself: _: {
    pysmartapp = pyself.callPackage ../pysmartapp/derivation.nix {};
    python-ecobee-api = pyself.callPackage ../python-ecobee-api/derivation.nix {};
    rachiopy = pyself.callPackage ../rachiopy/derivation.nix {};
    ring_doorbell = pyself.callPackage ../ring_doorbell/derivation.nix {};
    tuyaha = pyself.callPackage ../tuyaha/derivation.nix {};
  };
  extraPackages = ps:
    callPackage ../../config/modules/ui/home-assistant/packages.nix {
      inherit ps;
    };
}
