{lib, ...}: {
  options.backupDiskId = lib.mkOption {
    type = lib.types.str;
  };
}
