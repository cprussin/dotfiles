{
  pkgs,
  config,
  lib,
  ...
}: let
  passwords = pkgs.callPackage ../../../../../lib/passwords.nix {};

  sanitizedEnvVar = builtins.replaceStrings [" " "-"] ["_" "_"];

  peapMschapIdentityEnvVar = networkName: "${sanitizedEnvVar networkName}_USERNAME";
  peapMschapPasswordEnvVar = networkName: "${sanitizedEnvVar networkName}_PASSWORD";

  mkWpaNetworks = lib.mapAttrs (
    networkName: config:
      config // {pskRaw = "@${sanitizedEnvVar networkName}@";}
  );

  mkWpa3Networks = lib.mapAttrs (
    networkName: config:
      config
      // {
        authProtocols = lib.mkForce ["SAE" "FT-SAE"];
        auth = "sae_password=\"@${sanitizedEnvVar networkName}@\"";
        extraConfig = "ieee80211w=1";
      }
  );

  mkPeapMschapNetworks = lib.mapAttrs (
    networkName: config:
      config
      // {
        auth = ''
          proto=RSN
          key_mgmt=WPA-EAP
          pairwise=CCMP
          eap=PEAP
          identity="@${peapMschapIdentityEnvVar networkName}@"
          password="@${peapMschapPasswordEnvVar networkName}@"
          phase1="peaplabel=0"
          phase2="auth=MSCHAPV2"
        '';
      }
  );

  wpa3Networks = mkWpa3Networks {
    Centar.priority = 1;
    CentarPhone.priority = 2;
  };

  wpaNetworks = mkWpaNetworks {
    # Home networks
    CentarCar = {};

    # Friends' networks
    "PC House 3" = {};
    DeathStar5 = {};

    # Work
    Verkada-Guest = {};
  };

  peapMschapNetworks = mkPeapMschapNetworks {
    WeWorkWiFi = {};
  };

  insecureNetworks = {};
in {
  deployment.keys.wpa-passphrase-file = {
    keyCommand = passwords.getWpaPassphraseFile (
      lib.flatten (
        (
          map
          (network: ["--wpa3" network (sanitizedEnvVar network)])
          (builtins.attrNames wpa3Networks)
        )
        ++ (
          map
          (network: ["--wpa" network (sanitizedEnvVar network)])
          (builtins.attrNames wpaNetworks)
        )
        ++ (
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
    interfaces = [config.interfaces.wifi];
    environmentFile = config.deployment.keys.wpa-passphrase-file.path;
    networks = wpa3Networks // wpaNetworks // peapMschapNetworks // insecureNetworks;
  };
}
