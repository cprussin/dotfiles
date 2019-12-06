{ pkgs, config, lib, ... }:

{
  primary-user.home-manager = {
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
