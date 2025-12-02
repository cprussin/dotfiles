{
  pkgs,
  config,
  ...
}: let
  passwords = pkgs.callPackage ../../../lib/passwords.nix {};
  network = pkgs.callPackage ../../../lib/network.nix {};
in {
  deployment.keys = {
    "eyes.internal.prussin.net.crt" = {
      inherit (config.users.users.nginx) group;
      keyCommand = passwords.getFullPassword "Connor/Infrastructure/ssl/eyes.internal.prussin.net/cert";
      user = config.users.users.nginx.name;
    };

    "eyes.internal.prussin.net.key" = {
      inherit (config.users.users.nginx) group;
      keyCommand = passwords.getFullPassword "Connor/Infrastructure/ssl/eyes.internal.prussin.net/key";
      user = config.users.users.nginx.name;
    };
  };

  networking.firewall.interfaces = {
    prussinnet.allowedTCPPorts = [80 443];
  };

  services = {
    frigate = {
      enable = true;
      hostname = "eyes.internal.prussin.net";

      settings = {
        mqtt.enabled = true;

        record = {
          enabled = true;
          retain = {
            days = 7;
            mode = "all";
          };
        };

        cameras."backyard".ffmpeg.inputs = [
          {
            path = "rtsp://${network.home.camera-backyard.address}:554/h264Preview_01_sub";
            roles = ["audio" "detect" "record"];
          }
        ];
        cameras."driveway".ffmpeg.inputs = [
          {
            path = "rtsp://${network.home.camera-driveway.address}/h264Preview_01_sub";
            roles = ["audio" "detect" "record"];
          }
        ];
        cameras."front-door".ffmpeg.inputs = [
          {
            path = "rtsp://${network.home.camera-front-door.address}/h264Preview_01_sub";
            roles = ["audio" "detect" "record"];
          }
        ];
        cameras."front-steps".ffmpeg.inputs = [
          {
            path = "rtsp://${network.home.camera-front-steps.address}/h264Preview_01_sub";
            roles = ["audio" "detect" "record"];
          }
        ];
        cameras."garage".ffmpeg.inputs = [
          {
            path = "rtsp://${network.home.camera-garage.address}/h264Preview_01_sub";
            roles = ["audio" "detect" "record"];
          }
        ];
        cameras."grill".ffmpeg.inputs = [
          {
            path = "rtsp://${network.home.camera-grill.address}/h264Preview_01_sub";
            roles = ["audio" "detect" "record"];
          }
        ];
        cameras."laundry-door".ffmpeg.inputs = [
          {
            path = "rtsp://${network.home.camera-laundry-door.address}/h264Preview_01_sub";
            roles = ["audio" "detect" "record"];
          }
        ];
        cameras."side".ffmpeg.inputs = [
          {
            path = "rtsp://${network.home.camera-side.address}/h264Preview_01_sub";
            roles = ["audio" "detect" "record"];
          }
        ];
      };
    };

    nginx.virtualHosts."eyes.internal.prussin.net" = {
      listenAddresses = ["[${network.wireguard6.crux.address}]" "${network.wireguard4.crux.address}"];
      sslCertificate = "/run/keys/eyes.internal.prussin.net.crt";
      sslCertificateKey = "/run/keys/eyes.internal.prussin.net.key";
      forceSSL = true;
    };
  };

  systemd.services = {
    frigate = {
      requires = ["import-tank.service"];
      after = ["import-tank.service"];
    };
    nginx = {
      after = [
        "eyes.internal.prussin.net.crt-key.service"
        "eyes.internal.prussin.net.key-key.service"
      ];
      requires = [
        "eyes.internal.prussin.net.crt-key.service"
        "eyes.internal.prussin.net.key-key.service"
      ];
    };
  };
}
