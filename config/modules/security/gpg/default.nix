{ pkgs, config, lib, ... }:

{
  home-manager.users.${config.primary-user.name} = {
    home = {
      packages = lib.mkForce [ pkgs.gnupg ];
      sessionVariables.GNUPGHOME = config.secure.gnupg;
    };

    services.gpg-agent = {
      enable = true;
      enableSshSupport = true;
    };
  };
}
