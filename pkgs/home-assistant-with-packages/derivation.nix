{ home-assistant, callPackage, ... }@args:

home-assistant.override (
  (builtins.removeAttrs args [ "home-assistant" "callPackage" ]) // {
    packageOverrides = pyself: _: {
      aiogithubapi = pyself.callPackage ../python-modules/aiogithubapi/derivation.nix { };
      aiohomekit = pyself.callPackage ../python-modules/aiohomekit/derivation.nix { };
      gtts-token = pyself.callPackage ../python-modules/gtts-token/derivation.nix { };
      hacs-frontend = pyself.callPackage ../python-modules/hacs-frontend/derivation.nix { };
      pysmartapp = pyself.callPackage ../python-modules/pysmartapp/derivation.nix { };
      python-ecobee-api = pyself.callPackage ../python-modules/python-ecobee-api/derivation.nix { };
      queueman = pyself.callPackage ../python-modules/queueman/derivation.nix { };
      rachiopy = pyself.callPackage ../python-modules/rachiopy/derivation.nix { };
      ring_doorbell = pyself.callPackage ../python-modules/ring_doorbell/derivation.nix { };
    };
    extraPackages = ps:
      callPackage ../../config/modules/ui/home-assistant/packages.nix {
        inherit ps;
      };
  }
)
