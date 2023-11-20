{lib, ...}: {
  options.backupDisk = lib.mkOption {
    type = lib.types.submodule (
      {config, ...}: {
        options = {
          diskId = lib.mkOption {
            type = lib.types.str;
          };

          filenameBase = lib.mkOption {
            type = lib.types.str;
            default = builtins.replaceStrings [":"] [""] config.diskId;
            description = ''
              The base string used to construct the key files and systemd
              tasks for this drive.  Usually this is the same as the drive ID,
              but sometimes you may want to use a different name for some
              reason.
            '';
          };
        };
      }
    );
  };
}
