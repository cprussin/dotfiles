{ pkgs, config, ... }:

{
  networking.wireless = {
    enable = true;

    networks = {
      # Home networks
      Centar = {
        psk = builtins.extraBuiltins.password pkgs config "Wifi/Centar";
        priority = 1;
      };
      CentarPhone = {
        psk = builtins.extraBuiltins.password pkgs config "Wifi/CentarPhone";
        priority = 2;
      };
      CentarCar.psk = builtins.extraBuiltins.password pkgs config "Wifi/CentarCar";

      # Netflix networks
      Netflix = {};

      # Friends' networks
      "PC House2".psk = builtins.extraBuiltins.password pkgs config "Wifi/PC House2";

      # Hotel networks
      Courtyard_GUEST = {};
    };
  };
}
