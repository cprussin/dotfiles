{ pkgs, lib, config, ... }:

let
  secureOption = lib.mkOption {
    type = lib.types.submodule {
      options = {
        path = lib.mkOption {
          type = lib.types.str;
          default = "/secure";
          description = "The path to the secure directory.";
        };

        passwords = lib.mkOption {
          type = lib.types.str;
          default = "${config.secure.path}/passwords";
          description = "The path to the password store.";
        };

        gnupg = lib.mkOption {
          type = lib.types.str;
          default = "${config.secure.path}/gnupg";
          description = "The path to the gnupg home directory.";
        };
      };
    };

    default = {};
  };
in

{
  options.secure = secureOption;

  config.primary-user.sudo-cmds = [
    "${pkgs.utillinux}/bin/mount ${config.secure.path}"
    "${pkgs.utillinux}/bin/umount ${config.secure.path}"
  ];
}
