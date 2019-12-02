{ pkgs, config, lib, ... }:

{
  home-manager.users.${config.primary-user.name}.home = {
    packages = lib.mkForce [ pkgs.pass ];
    sessionVariables.PASSWORD_STORE_DIR = config.secure.passwords;
  };
}
