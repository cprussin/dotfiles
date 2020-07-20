{ config, lib, pkgs, ... }:

let
  cfg = config.detachedLuksWithNixopsKeys;

  base64Decode = path: "${pkgs.coreutils}/bin/base64 -d ${path}";

  keys =
    lib.mapAttrs'
      (k: v: lib.nameValuePair "${k}-key" { text = v.key; })
      cfg;

  headers =
    lib.mapAttrs'
      (k: v: lib.nameValuePair "${k}-header" { text = v.header; })
      cfg;

  mkUnlockScript = drive: pkgs.writeShellScript "unlock-${drive}" ''
    ${pkgs.coreutils}/bin/mkdir -p /tmp/${drive}
    ${pkgs.utillinux}/bin/mount -t tmpfs tmpfs /tmp/${drive}
    ${base64Decode config.deployment.keys."${drive}-header".path} > /tmp/${drive}/header

    ${pkgs.cryptsetup}/bin/cryptsetup open \
      --key-file <(${base64Decode config.deployment.keys."${drive}-key".path}) \
      --header /tmp/${drive}/header \
      /dev/disk/by-id/${drive} crypt-${drive}

    ${pkgs.coreutils}/bin/shred -u /tmp/${drive}/header
    ${pkgs.utillinux}/bin/umount /tmp/${drive}
    ${pkgs.coreutils}/bin/rmdir /tmp/${drive}
  '';
in

{
  options.detachedLuksWithNixopsKeys = lib.mkOption {
    default = null;
    description = ''
      An attrset mapping drive IDs to luks keys & headers to unlock those
      drives.  For each drive, a systemd unit named unlock-<drive>.service will
      be created.
    '';
    type = lib.types.nullOr (
      lib.types.attrsOf (
        lib.types.submodule {
          options = {
            key = lib.mkOption {
              type = lib.types.str;
              description = ''
                The base64-encoded contents of the luks key for this drive
              '';
            };
            header = lib.mkOption {
              type = lib.types.str;
              description = ''
                The base64-encoded contents of the luks header for this drive
              '';
            };
          };
        }
      )
    );
  };

  config = lib.mkIf (cfg != null) {
    deployment.keys = keys // headers;

    systemd.services = lib.mapAttrs' (
      drive: _:
        lib.nameValuePair "unlock-${drive}" {
          enable = true;
          description = "Unlock encrypted device ${drive}.";
          wantedBy = [ "zfs.target" ];
          after = [
            "${drive}-key-key.service"
            "${drive}-header-key.service"
          ];
          wants = [
            "${drive}-key-key.service"
            "${drive}-header-key.service"
          ];
          serviceConfig = {
            ExecStart = mkUnlockScript drive;
            Type = "oneshot";
          };
        }
    ) cfg;
  };
}
