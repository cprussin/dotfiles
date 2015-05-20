{ config, pkgs, lib, ... }:

{
  options.primaryUserName = lib.mkOption {
    type = lib.types.str;
    description = "Specify the primary username for this computer.";
  };

  config.users.users.${config.primaryUserName} = {
    isNormalUser = true;
    uid = 1000;
    shell = pkgs.zsh;
  };
}
