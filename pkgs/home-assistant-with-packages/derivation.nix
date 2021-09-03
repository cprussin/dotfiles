{ home-assistant, callPackage, fetchFromGitHub, ... }:

(home-assistant.override {
  packageOverrides = pyself: pysuper: {
    aiogithubapi = pyself.callPackage ../python-modules/aiogithubapi/derivation.nix { };
    hacs-frontend = pyself.callPackage ../python-modules/hacs-frontend/derivation.nix { };
    queueman = pyself.callPackage ../python-modules/queueman/derivation.nix { };

    # TODO remove me when the home-assistant version revs
    aioesphomeapi = pysuper.aioesphomeapi.overrideAttrs (_: {
      version = "7.0.0";
      name = "aioesphomeapi-7.0.0";
      src = fetchFromGitHub {
        owner = "esphome";
        repo = "aioesphomeapi";
        rev = "v7.0.0";
        sha256 = "sha256-ho/1fpq4yAgmYNERPqs51oqr08ncaN9+GRTUUuGU7ps=";
      };
    });

  };
  extraPackages = ps:
    callPackage ../../config/modules/ui/home-assistant/packages.nix {
      inherit ps;
    };
}).overrideAttrs (_: {
  doInstallCheck = false;
})
