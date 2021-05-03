{ ... }:

{
  imports = [
    ./hardware.nix
    ./backup.nix
    ./plex.nix
    ./dynamic-dns.nix
    ./acme.nix
    ./library.nix
    ../../profiles/physical-machine
    ../../profiles/server
    ../../modules/ui/home-assistant
  ];

  primary-user.name = "cprussin";
  networking = {
    hostName = "crux";
    hostId = "a362c6ea";
  };
  environment.etc."machine-id".text = "bf6ba660172042baa958c54739b5fdb9\n";
  services = {
    mingetty.greetingLine = builtins.readFile ./greeting;
    home-assistant.virtualHost = "720-natoma-drive.prussin.net";
  };

  systemd.services = {
    home-assistant = {
      wants = [
        "import-tank.service"
      ];
      after = [
        "import-tank.service"
      ];
    };
    syncthing = {
      wants = [
        "import-tank.service"
      ];
      after = [
        "import-tank.service"
      ];
    };
  };

  programs.powerpanel.enable = true;
}
