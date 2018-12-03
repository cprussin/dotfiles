{ ... }:

let
  passwords = toString ../../data/nogit/secrets/wifi;
  getpassword = network: builtins.readFile (passwords + "/" + network);
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
      strmdev = {
        psk = getpassword "strmdev";
        priority = 2;
      };
      Netflix.priority = 1;

      # Friends' networks
      "PC House2".psk = getpassword "PC House2";

      # Hotel networks
      Courtyard_GUEST = {};
    };
  };
}
