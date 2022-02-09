{ pkgs, config, lib, ... }:

let
  passwords = pkgs.callPackage ../../../../../lib/passwords.nix { };

  sanitizedEnvVar = builtins.replaceStrings [ " " ] [ "_" ];

  peapMschapIdentityEnvVar = networkName: "${sanitizedEnvVar networkName}_USERNAME";
  peapMschapPasswordEnvVar = networkName: "${sanitizedEnvVar networkName}_PASSWORD";

  mkWpaNetworks = lib.mapAttrs (networkName: config:
    config // { pskRaw = "@${sanitizedEnvVar networkName}@"; }
  );

  mkPeapMschapNetworks = lib.mapAttrs (networkName: config:
    config // {
      auth = ''
        proto=RSN
        key_mgmt=WPA-EAP
        pairwise=CCMP
        eap=PEAP
        identity="@${peapMschapIdentityEnvVar networkName}@"
        password=@${peapMschapPasswordEnvVar networkName}@
        phase1="peaplabel=0"
        phase2="auth=MSCHAPV2"
      '';
    }
  );

  wpaNetworks = mkWpaNetworks {
    # Home networks
    Centar = { priority = 1; };
    CentarPhone = { priority = 2; };
    CentarCar = { };

    # Friends' networks
    "PC House2" = { };
    DeathStar5 = { };
  };

  peapMschapNetworks = mkPeapMschapNetworks {
    WeWorkWiFi = { };
  };

  insecureNetworks = { };
in

{
  deployment.keys.wpa-passphrase-file = {
    keyCommand = passwords.getWpaPassphraseFile (
      lib.flatten (
        (
          map
            (network: [ "--wpa" network (sanitizedEnvVar network) ])
            (builtins.attrNames wpaNetworks)
        ) ++
        (
          map
            (network: [
              "--peap-mschap"
              network
              (peapMschapIdentityEnvVar network)
              (peapMschapPasswordEnvVar network)
            ])
            (builtins.attrNames peapMschapNetworks)
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
    networks = wpaNetworks // peapMschapNetworks // insecureNetworks;
  };
}
