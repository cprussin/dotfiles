{ config, ... }:

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

    syncthing.declarative.folders = {
      DCIM = {
        path = "${config.primary-user.home}/Phone/DCIM";
        devices = [ "pegasus" ];
      };
      Pictures = {
        path = "${config.primary-user.home}/Phone/Pictures";
        devices = [ "pegasus" ];
      };
      Snapchat = {
        path = "${config.primary-user.home}/Phone/Snapchat";
        devices = [ "pegasus" ];
      };
      Telegram = {
        path = "${config.primary-user.home}/Phone/Telegram";
        devices = [ "pegasus" ];
      };
      Total_Launcher_Backups = {
        path = "${config.primary-user.home}/Phone/Total Launcher Backups";
        devices = [ "pegasus" ];
      };
      WhatsApp_Media = {
        path = "${config.primary-user.home}/Phone/WhatsApp Media";
        devices = [ "pegasus" ];
      };
    };
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
