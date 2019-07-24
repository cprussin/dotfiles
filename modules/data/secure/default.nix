{ lib, config, ... }:

{
  options.secure = lib.mkOption {
    type = lib.types.submodule {
      options = {
        path = lib.mkOption {
          type = lib.types.str;
          default = "~/.secure";
          description = "The path to the secure directory.";
        };

        passwords = lib.mkOption {
          type = lib.types.str;
          default = "${config.secure.path}/passwords";
          description = "The path to the password store.";
        };
      };
    };

    default = {};
  };
}
