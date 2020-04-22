{ pkgs, lib, ... }:

{
  primary-user.home-manager = {
    home.packages = lib.mkForce [ pkgs.gnupg ];

    services.gpg-agent = {
      enable = true;
      enableSshSupport = true;
      sshKeys = [ "828682F09BD928BC5B3285F89A8B9FBFCBD05482 0" ];
    };
  };
}
