_: {
  boot.supportedFilesystems = ["zfs"];
  services.zfs = {
    trim.enable = true;
    autoSnapshot = {
      enable = true;
      flags = "-k -p --utc";
    };
    autoScrub = {
      enable = true;
      interval = "Sun, 02:00";
      randomizedDelaySec = "1h";
    };
  };
}
