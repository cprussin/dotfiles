{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.secure;

  secures = builtins.attrValues cfg;

  pools = map (opts: opts.pool) secures;

  mapConcatPools = cmd: sep: lib.concatStringsSep sep (map cmd pools);

  plural = thing:
    if builtins.length thing == 1
    then ""
    else "s";

  awaitingMsg = "Waiting for secure filesystem${plural pools} to appear..";

  closingMsg = "Closing secure filesystem${plural pools}...";

  zpool = "${pkgs.zfs}/bin/zpool";

  isPoolAvailable = pool: "(${zpool} import 2>/dev/null | grep 'pool: ${pool}' 2>&1 >/dev/null)";

  importPool = pool: "${zpool} import -o readonly=on '${pool}'";

  exportPool = pool: "${zpool} export '${pool}'";

  somePoolsUnavailable = "(! (${mapConcatPools isPoolAvailable " && "}))";

  mkLuksDevice = luksBase: drive: {
    name = "crypt-${drive}";
    value = {
      device = "/dev/disk/by-id/${drive}";
      keyFile = "${luksBase}/${config.networking.hostName}/${drive}/key";
      header = "${luksBase}/${config.networking.hostName}/${drive}/header";
    };
  };

  luksDevicesFor = secure:
    builtins.listToAttrs (map (mkLuksDevice secure.luks) secure.luksDrives);
in {
  options.secure = lib.mkOption {
    description = ''
      An attrset mapping usernames to their secure storage configurations.
      Secure storage configurations consist of a root path, along with paths in
      the root pointing to password and gpg storage.
    '';
    default = null;
    type = lib.types.nullOr (lib.types.attrsOf (
      lib.types.submodule (
        {config, ...}: {
          options = {
            pool = lib.mkOption {
              type = lib.types.str;
              description = "ZFS pool for secure storage.";
            };

            passwords = lib.mkOption {
              type = lib.types.str;
              description = "The path to the password store.";
            };

            gnupg = lib.mkOption {
              type = lib.types.str;
              description = "The path to the gnupg directory.";
            };

            luks = lib.mkOption {
              type = lib.types.str;
              description = "The path to the luks header & key directory.";
            };

            luksDrives = lib.mkOption {
              type = lib.types.listOf lib.types.str;
              default = [];
              description = ''
                The list of drive IDs whose luks keys & headers are stored in
                this volume.
              '';
            };

            importCmd = lib.mkOption {
              type = lib.types.path;
              description = ''
                The passwordless-sudo-enabled command to import the dataset
                read-only.
              '';
            };

            exportCmd = lib.mkOption {
              type = lib.types.path;
              description = ''
                The passwordless-sudo-enabled command to export the dataset.
              '';
            };
          };

          config = let
            pool = config.pool;
          in {
            passwords = lib.mkDefault "/${pool}/passwords";
            gnupg = lib.mkDefault "/${pool}/gnupg";
            luks = lib.mkDefault "/${pool}/crypt";
            importCmd = pkgs.writeShellScript "import-${pool}" (importPool pool);
            exportCmd = pkgs.writeShellScript "export-${pool}" (exportPool pool);
          };
        }
      )
    ));
  };

  config = lib.mkIf (cfg != null) {
    home-manager.users = lib.mapAttrs
    (
      _: secure: {config, ...}: {
        home = {
          sessionVariables.PASSWORD_STORE_DIR = secure.passwords;
          file = {
            ".gnupg/crls.d".source = config.lib.file.mkOutOfStoreSymlink "${secure.gnupg}/crls.d";
            ".gnupg/openpgp-revocs.d".source = config.lib.file.mkOutOfStoreSymlink "${secure.gnupg}/openpgp-revocs.d";
            ".gnupg/private-keys-v1.d".source = config.lib.file.mkOutOfStoreSymlink "${secure.gnupg}/private-keys-v1.d";
            ".gnupg/pubring.kbx".source = config.lib.file.mkOutOfStoreSymlink "${secure.gnupg}/pubring.kbx";
            ".gnupg/random_seed".source = config.lib.file.mkOutOfStoreSymlink "${secure.gnupg}/random_seed";
            ".gnupg/tofu.db".source = config.lib.file.mkOutOfStoreSymlink "${secure.gnupg}/tofu.db";
            ".gnupg/trustdb.gpg".source = config.lib.file.mkOutOfStoreSymlink "${secure.gnupg}/trustdb.gpg";
          };
        };
      }
    )
    cfg;

    sudo-cmds = lib.mapAttrs (_: opts: map toString [opts.importCmd opts.exportCmd]) cfg;

    boot.initrd = lib.mkIf (lib.any (secure: (builtins.length secure.luksDrives > 0)) secures) {
      kernelModules = ["usb_storage" "loop"];

      preLVMCommands = (
        lib.mkMerge [
          (
            lib.mkBefore ''
              echo -n "${awaitingMsg}"
              while ${somePoolsUnavailable}
              do
                echo -n "."
                sleep 0.25
              done
              echo -n " done!"
              echo
              ${mapConcatPools importPool "\n"}
            ''
          )

          (
            lib.mkAfter ''
              echo "${closingMsg}"
              ${mapConcatPools exportPool "\n"}
            ''
          )
        ]
      );

      luks.devices =
        lib.foldl (acc: secure: acc // (luksDevicesFor secure)) {} secures;
    };
  };
}
