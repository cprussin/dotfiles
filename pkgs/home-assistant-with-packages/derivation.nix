{ home-assistant, callPackage, ... }:

(home-assistant.override {
  packageOverrides = pyself: _: {
    aiogithubapi = pyself.callPackage ../python-modules/aiogithubapi/derivation.nix { };
    hacs-frontend = pyself.callPackage ../python-modules/hacs-frontend/derivation.nix { };
    queueman = pyself.callPackage ../python-modules/queueman/derivation.nix { };
  };
  extraPackages = ps:
    callPackage ../../config/modules/ui/home-assistant/packages.nix {
      inherit ps;
    };
}).overrideAttrs (_: {
  doInstallCheck = false;
})
