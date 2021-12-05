{ pkgs, config, lib, ... }:

let
  passwords = pkgs.callPackage ../../../../../lib/passwords.nix { };

  pskEnvVarName = builtins.replaceStrings [ " " ] [ "_" ];

  mkSecureNetworks = lib.mapAttrs (networkName: config:
    config // { pskRaw = "@${pskEnvVarName networkName}@"; }
  );

  secureNetworks = mkSecureNetworks {
    # Home networks
    Centar = { priority = 1; };
    CentarPhone = { priority = 2; };
    CentarCar = { };

    # Friends' networks
    "PC House2" = { };
  };

  insecureNetworks = { };
in

{
  deployment.keys.wpa-passphrase-file = {
    keyCommand = passwords.getWpaPassphraseFile (
      lib.flatten (
        map (network: [ network (pskEnvVarName network) ]) (
          builtins.attrNames secureNetworks
        )
      )
    );
    destDir = "/secrets";
  };

  networking.wireless = {
    enable = true;
    userControlled.enable = true;
    interfaces = [ config.interfaces.wifi ];
    environmentFile = config.deployment.keys.wpa-passphrase-file.path;
    networks = secureNetworks // insecureNetworks;
  };
}
