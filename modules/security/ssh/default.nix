{ config, lib, pkgs, ... }:

let
  key = name: "${config.sshKeys}/${name}";
in

{
  options.sshKeys = lib.mkOption {
    type = lib.types.str;
    default = "${config.secrets}/ssh";
    description = "A string containing the path to the ssh keys directory";
  };

  config = {
    home.file.".ssh/known_hosts".source = ./known_hosts;
    home.file.".ssh/authorized_keys".source = ./authorized_keys;
    home.file.".ssh/environment".text = "PATH=${pkgs.git}/bin";

    programs.ssh = {
      enable = true;
      matchBlocks = [
        {
          host = "home.prussin.net 10.0.0.11";
          user = "cprussin";
          port = 5822;
          identityFile = key "home.prussin.net";
        }
        {
          host = "prussin.net *.bci-incorporated.com";
          user = "connor";
          port = 3580;
          identityFile = key "prussin.net";
        }
        {
          host = "*.prussin.net songoftheday.net";
          user = "root";
          port = 7534;
          identityFile = key "new.prussin.net";
        }
        {
          host = "*github*";
          identityFile = key "github";
        }
        {
          host = "aur.archlinux.org";
          user = "aur";
          identityFile = key "aur";
        }
        {
          host = "stash.corp.netflix.com";
          identityFile = key "netflix";
        }
        {
          host = "aws*.prod.netflix.net aws*.test.netflix.net aws*.mgmt.netflix.net awstest awsprod awsmgmt";
          identityFile = key "metatron";
        }
      ];
    };
  };
}
