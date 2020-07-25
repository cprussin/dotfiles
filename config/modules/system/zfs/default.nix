{ ... }:

{
  boot.supportedFilesystems = [ "zfs" ];
  services.zfs = {
    autoSnapshot = {
      enable = true;
      flags = "-k -p --utc";
    };
    autoScrub.enable = true;
  };
}
