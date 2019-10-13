{ pkgs, ... }:

let
  getpassword = network: builtins.extraBuiltins.pass pkgs ("Wifi/" + network);
in

{
  networking.wireless = {
    enable = true;

    networks = {
      # Home networks
      Centar = {
        psk = getpassword "Centar";
        priority = 1;
      };
      CentarPhone = {
        psk = getpassword "CentarPhone";
        priority = 2;
      };
      CentarCar.psk = getpassword "CentarCar";

      # Netflix networks
      Netflix = {};

      # Friends' networks
      "PC House2".psk = getpassword "PC House2";

      # Hotel networks
      Courtyard_GUEST = {};
    };
  };
}
