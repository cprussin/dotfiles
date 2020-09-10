{ ... }:

{
  boot.supportedFilesystems = [ "zfs" ];
  services.zfs = {
    trim.enable = true;
    autoSnapshot = {
      enable = true;
      flags = "-k -p --utc";
    };
    autoScrub.enable = true;
  };
}
