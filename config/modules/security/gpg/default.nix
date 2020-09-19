{ pkgs, lib, ... }:

{
  primary-user.home-manager = { config, ... }: {
    home.packages = lib.mkForce [ pkgs.gnupg ];

    services.gpg-agent = {
      enable = true;
      enableSshSupport = true;
      sshKeys = [ "828682F09BD928BC5B3285F89A8B9FBFCBD05482 0" ];
    };

    home.file = {
      ".gnupg/crls.d".source = config.lib.file.mkOutOfStoreSymlink /secure/gnupg/crls.d;
      ".gnupg/openpgp-revocs.d".source = config.lib.file.mkOutOfStoreSymlink /secure/gnupg/openpgp-revocs.d;
      ".gnupg/private-keys-v1.d".source = config.lib.file.mkOutOfStoreSymlink /secure/gnupg/private-keys-v1.d;
      ".gnupg/pubring.kbx".source = config.lib.file.mkOutOfStoreSymlink /secure/gnupg/pubring.kbx;
      ".gnupg/random_seed".source = config.lib.file.mkOutOfStoreSymlink /secure/gnupg/random_seed;
      ".gnupg/tofu.db".source = config.lib.file.mkOutOfStoreSymlink /secure/gnupg/tofu.db;
      ".gnupg/trustdb.gpg".source = config.lib.file.mkOutOfStoreSymlink /secure/gnupg/trustdb.gpg;
    };
  };
}
