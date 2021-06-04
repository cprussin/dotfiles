{ home-assistant, callPackage, ... }@args:

(home-assistant.override (
  (builtins.removeAttrs args [ "home-assistant" "callPackage" ]) // {
    packageOverrides = pyself: _: {
      aiogithubapi = pyself.callPackage ../python-modules/aiogithubapi/derivation.nix { };
      hacs-frontend = pyself.callPackage ../python-modules/hacs-frontend/derivation.nix { };
      python-ecobee-api = pyself.callPackage ../python-modules/python-ecobee-api/derivation.nix { };
      queueman = pyself.callPackage ../python-modules/queueman/derivation.nix { };
      pychromecast = pyself.callPackage ../python-modules/pychromecast/derivation.nix { };
      ring_doorbell = pyself.callPackage ../python-modules/ring_doorbell/derivation.nix { };
    };
    extraPackages = ps:
      callPackage ../../config/modules/ui/home-assistant/packages.nix {
        inherit ps;
      };
  }
)).overrideAttrs (_: {
  doInstallCheck = false;
})
