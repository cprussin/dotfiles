{ home-assistant, callPackage, ... }@args:

home-assistant.override (
  (builtins.removeAttrs args [ "home-assistant" "callPackage" ]) // {
    packageOverrides = pyself: _: {
      aiogithubapi = pyself.callPackage ../aiogithubapi/derivation.nix {};
      hacs-frontend = pyself.callPackage ../hacs-frontend/derivation.nix {};
      pysmartapp = pyself.callPackage ../pysmartapp/derivation.nix {};
      python-ecobee-api = pyself.callPackage ../python-ecobee-api/derivation.nix {};
      queueman = pyself.callPackage ../queueman/derivation.nix {};
      rachiopy = pyself.callPackage ../rachiopy/derivation.nix {};
      ring_doorbell = pyself.callPackage ../ring_doorbell/derivation.nix {};
    };
    extraPackages = ps:
      callPackage ../../config/modules/ui/home-assistant/packages.nix {
        inherit ps;
      };
  }
)
