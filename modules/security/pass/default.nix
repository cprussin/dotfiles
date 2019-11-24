{ pkgs, config, lib, ... }:

{
  home-manager.users.${config.primaryUserName}.home = {
    packages = lib.mkForce [ pkgs.pass ];
    sessionVariables.PASSWORD_STORE_DIR = config.secure.passwords;
  };
}
