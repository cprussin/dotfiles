{ config, lib, ... }:
let
  cfg = config.secure;
in
{
  options.secure = lib.mkOption {
    description = ''
      An attrset mapping usernames to their secure storage configurations.
      Secure storage configurations consist of a root path, along with paths in
      the root pointing to password and gpg storage.
    '';
    default = { };
    type = lib.types.attrsOf (
      lib.types.submodule (
        { config, ... }: {
          options = {
            mountPoint = lib.mkOption {
              type = lib.types.str;
              description = "Location of the mounted the file system.";
            };

            device = lib.mkOption {
              default = null;
              type = lib.types.nullOr lib.types.str;
              description = "Location of the device.";
            };

            fsType = lib.mkOption {
              default = "auto";
              type = lib.types.str;
              description = "Type of the file system.";
            };

            options = lib.mkOption {
              default = [ "defaults" ];
              description = "Options used to mount the file system.";
              type = lib.types.listOf lib.types.str;
            };

            passwords = lib.mkOption {
              type = lib.types.str;
              description = "The path to the password store.";
            };

            gnupg = lib.mkOption {
              type = lib.types.str;
              description = "The path to the gnupg directory.";
            };
          };

          config = {
            passwords = lib.mkDefault "${config.mountPoint}/passwords";
            gnupg = lib.mkDefault "${config.mountPoint}/gnupg";
          };
        }
      )
    );
  };

  config = {
    home-manager.users = lib.mapAttrs
      (
        _: secure: { config, ... }: {
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

    sudo-cmds = lib.mapAttrs
      (
        _: secure: [
          "${config.security.wrapperDir}/mount ${secure.mountPoint}"
          "${config.security.wrapperDir}/umount ${secure.mountPoint}"
        ]
      )
      cfg;

    fileSystems = lib.mapAttrs'
      (
        _: secure:
          lib.nameValuePair secure.mountPoint {
            device = secure.device;
            fsType = secure.fsType;
            options = secure.options;
          }
      )
      cfg;
  };
}
