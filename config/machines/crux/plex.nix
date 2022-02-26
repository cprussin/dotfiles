{...}: {
  services.plex = {
    enable = true;
    openFirewall = true;
    managePlugins = true;
  };

  systemd.services.plex = {
    wants = [
      "import-tank.service"
    ];
    after = [
      "import-tank.service"
    ];
  };
}
