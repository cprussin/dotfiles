{
  config,
  pkgs,
  lib,
  ...
}: let
  passwords = pkgs.callPackage ../../../lib/passwords.nix {};
  network = pkgs.callPackage ../../../lib/network.nix {};

  gemini-public-key = "hmvFMJuINO7N6EO04Pnf32FJFaW5N0zDctot3Qpo8gg=";
  pegasus-public-key = "tp9/x6jGpecKdUQ2y0Kgv51/3eXP0b3smroUt87Jx3g=";
  shauna-phone-public-key = "tifJWMO8/P4B5QDNpieB/CvLtCyvtuoXi5GnAQYywRU=";

  wireguardListenPort = 45676;
in {
  imports = [
    ./hardware.nix
    ./backup.nix
    ./plex.nix
    ./dynamic-dns.nix
    ./acme.nix
    ./library.nix
    ./matrix.nix
    ./reece-access.nix
    ./dvd-ripping.nix
    ../../profiles/physical-machine
    ../../profiles/server
  ];

  deployment.keys = {
    wireguard-private-key.keyCommand = passwords.getPassword "Connor/Infrastructure/wireguard/crux/private-key";
    gemini-preshared-key.keyCommand = passwords.getPassword "Connor/Infrastructure/wireguard/gemini/psk";
    pegasus-preshared-key.keyCommand = passwords.getPassword "Connor/Infrastructure/wireguard/pegasus/psk";
    shauna-phone-preshared-key.keyCommand = passwords.getPassword "Connor/Infrastructure/wireguard/shauna-phone/psk";
    "home-assistant.pruss.in.crt" = {
      keyCommand = passwords.getFullPassword "Connor/Infrastructure/ssl/home-assistant.pruss.in/cert";
      user = "nginx";
      group = "nginx";
    };
    "home-assistant.pruss.in.key" = {
      keyCommand = passwords.getFullPassword "Connor/Infrastructure/ssl/home-assistant.pruss.in/key";
      user = "nginx";
      group = "nginx";
    };
  };

  containers = {
    coredns = {
      autoStart = true;
      privateNetwork = true;
      hostAddress = network.nixos-container-host-addresses.coredns;
      localAddress = network.nixos-containers.coredns;
      config = {
        networking.firewall = {
          allowedTCPPorts = [53];
          allowedUDPPorts = [53];
        };
        services.coredns = {
          enable = true;
          config = ''
            .:53 {
              forward . 1.1.1.1 1.0.0.1
              log
              errors
            }

            pruss.in:53 {
              file ${
              pkgs.writeText "db.pruss.in" ''
                $ORIGIN pruss.in.
                @	3600 IN	SOA sns.dns.icann.org. noc.dns.icann.org. (
                        2017042745 ; serial
                        7200       ; refresh (2 hours)
                        3600       ; retry (1 hour)
                        1209600    ; expire (2 weeks)
                        3600       ; minimum (1 hour)
                )

                3600 IN NS a.iana-servers.net.
                3600 IN NS b.iana-servers.net.

                home-assistant     IN A     ${network.nixos-containers.home-assistant-proxy}
                ${
                  lib.concatStringsSep "\n" (
                    lib.mapAttrsToList (host: ip: "${host} IN A ${ip}") network.wireguard
                  )
                }
              ''
            }
              log
              errors
            }
          '';
        };
      };
    };
    home-assistant-proxy = {
      autoStart = true;
      privateNetwork = true;
      hostAddress = network.nixos-container-host-addresses.home-assistant-proxy;
      localAddress = network.nixos-containers.home-assistant-proxy;
      bindMounts."/run/keys".hostPath = "/run/keys";
      config = {
        networking.firewall.allowedTCPPorts = [80 443];
        users.users.nginx.extraGroups = ["keys"];
        services.nginx = {
          enable = true;
          recommendedTlsSettings = true;
          recommendedOptimisation = true;
          recommendedGzipSettings = true;
          recommendedProxySettings = true;
          virtualHosts."home-assistant.pruss.in" = {
            sslCertificate = "/run/keys/home-assistant.pruss.in.crt";
            sslCertificateKey = "/run/keys/home-assistant.pruss.in.key";
            forceSSL = true;
            extraConfig = "proxy_buffering off;";
            locations."/" = {
              proxyPass = "http://${network.oci-containers.home-assistant}:8123";
              proxyWebsockets = true;
            };
          };
        };
      };
    };
  };

  virtualisation.oci-containers = {
    containers = {
      home-assistant = {
        image = "ghcr.io/home-assistant/home-assistant:stable";
        environment.TZ = "America/Los_Angeles";
        extraOptions =
          [
            "--network=oci-containers"
            "--ip=${network.oci-containers.home-assistant}"
            "--device=/dev/ttyACM0:/dev/ttyACM0"
          ]
          ++ lib.mapAttrsToList (host: ip: "--add-host=${host}:${ip}") network.iot;
        volumes = ["/var/lib/hass:/config"];
      };
    };
  };

  primary-user.name = "cprussin";
  networking = {
    hostName = "crux";
    hostId = "a362c6ea";
    nat = {
      enable = true;
      internalInterfaces = ["ve-coredns+"];
      externalInterface = "enp7s0";
    };
    firewall.allowedUDPPorts = [wireguardListenPort];

    wireguard = {
      enable = true;
      interfaces.prussinnet = {
        #interfaceNamespace = "prussinnet";
        listenPort = wireguardListenPort;
        #preSetup = ''
        #  ip netns add prussinnet
        #  ip link add prussinnet-host type veth peer name prussinnet-wg netns prussinnet
        #'';
        #postShutdown = ''
        #  ip netns delete prussinnet
        #'';
        privateKeyFile = config.deployment.keys.wireguard-private-key.path;
        ips = ["${network.wireguard.crux}/24"];
        peers = [
          {
            allowedIPs = ["${network.wireguard.gemini}/32"];
            publicKey = gemini-public-key;
            presharedKeyFile = config.deployment.keys.gemini-preshared-key.path;
          }
          {
            allowedIPs = ["${network.wireguard.pegasus}/32"];
            publicKey = pegasus-public-key;
            presharedKeyFile = config.deployment.keys.pegasus-preshared-key.path;
          }
          {
            allowedIPs = ["${network.wireguard.shauna-phone}/32"];
            publicKey = shauna-phone-public-key;
            presharedKeyFile = config.deployment.keys.shauna-phone-preshared-key.path;
          }
        ];
      };
    };
  };
  environment.etc."machine-id".text = "bf6ba660172042baa958c54739b5fdb9\n";
  services = {
    getty.greetingLine = builtins.readFile ./greeting;
  };

  systemd.services = {
    oci-containers-network.script = ''
      ${pkgs.podman}/bin/podman network create --subnet=${network.oci-containers.id}/24 oci-containers
    '';
    wireguard-prussinnet = {
      after = ["wireguard-private-key-key.service" "gemini-preshared-key-key.service" "pegasus-preshared-key-key.service" "shauna-phone-preshared-key-key.service"];
      wants = ["wireguard-private-key-key.service" "gemini-preshared-key-key.service" "pegasus-preshared-key-key.service" "shauna-phone-preshared-key-key.service"];
    };
    "container@home-assistant-proxy" = {
      after = ["home-assistant.pruss.in.crt-key.service" "home-assistant.pruss.in.key-key.service"];
      wants = ["home-assistant.pruss.in.crt-key.service" "home-assistant.pruss.in.key-key.service"];
    };
    podman-home-assistant = {
      wants = ["import-tank.service" "oci-containers-network.service"];
      after = ["import-tank.service" "oci-containers-network.service"];
    };
    syncthing = {
      wants = ["import-tank.service"];
      after = ["import-tank.service"];
    };
    postgresql = {
      wants = ["import-tank.service"];
      after = ["import-tank.service"];
    };
  };

  programs.powerpanel.enable = true;
}
