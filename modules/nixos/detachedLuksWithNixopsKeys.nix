{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.detachedLuksWithNixopsKeys;

  base64Decode = path: "${pkgs.coreutils}/bin/base64 -d ${path}";

  keys =
    lib.mapAttrs'
    (_: v: lib.nameValuePair "${v.filenameBase}-key" {keyCommand = v.key;})
    cfg;

  headers =
    lib.mapAttrs'
    (_: v: lib.nameValuePair "${v.filenameBase}-header" {keyCommand = v.header;})
    cfg;

  mkUnlockScript = drive: filenameBase: ''
    ${pkgs.coreutils}/bin/mkdir -p /tmp/${filenameBase}
    ${config.security.wrapperDir}/mount -t tmpfs tmpfs /tmp/${filenameBase}
    ${base64Decode config.deployment.keys."${filenameBase}-header".path} > /tmp/${filenameBase}/header

    ${pkgs.cryptsetup}/bin/cryptsetup open \
      --key-file <(${base64Decode config.deployment.keys."${filenameBase}-key".path}) \
      --header /tmp/${filenameBase}/header \
      /dev/disk/by-id/${drive} crypt-${filenameBase}

    ${pkgs.coreutils}/bin/shred -u /tmp/${filenameBase}/header
    ${config.security.wrapperDir}/umount /tmp/${filenameBase}
    ${pkgs.coreutils}/bin/rmdir /tmp/${filenameBase}
  '';
in {
  options.detachedLuksWithNixopsKeys = lib.mkOption {
    default = null;
    description = ''
      An attrset mapping drive IDs to luks keys & headers to unlock those
      drives.  For each drive, a systemd unit named unlock-<drive>.service will
      be created.
    '';
    type = lib.types.nullOr (
      lib.types.attrsOf (
        lib.types.submodule (
          {name, ...}: {
            options = {
              key = lib.mkOption {
                type = lib.types.listOf lib.types.str;
                description = ''
                  The base64-encoded contents of the luks key for this drive
                '';
              };
              header = lib.mkOption {
                type = lib.types.listOf lib.types.str;
                description = ''
                  The base64-encoded contents of the luks header for this drive
                '';
              };
              filenameBase = lib.mkOption {
                type = lib.types.str;
                default = builtins.replaceStrings [":"] [""] name;
                description = ''
                  The base string used to construct the key files and systemd
                  tasks for this drive.  Usually this is the same as the drive ID,
                  but sometimes you may want to use a different name for some
                  reason.
                '';
              };
            };
          }
        )
      )
    );
  };

  config = lib.mkIf (cfg != null) {
    deployment.keys = keys // headers;

    systemd.services = lib.mapAttrs'
    (
      drive: opts:
        lib.nameValuePair "unlock-${opts.filenameBase}" {
          description = "Unlock encrypted device ${drive}.";
          after = [
            "${opts.filenameBase}-key-key.service"
            "${opts.filenameBase}-header-key.service"
          ];
          wants = [
            "${opts.filenameBase}-key-key.service"
            "${opts.filenameBase}-header-key.service"
          ];
          script = mkUnlockScript drive opts.filenameBase;
          preStop = "${pkgs.cryptsetup}/bin/cryptsetup close crypt-${opts.filenameBase}";
          serviceConfig = {
            RemainAfterExit = true;
            Type = "oneshot";
          };
        }
    )
    cfg;
  };
}
