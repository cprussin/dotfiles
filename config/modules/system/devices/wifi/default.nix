{ pkgs, ... }:

let
  passwords = pkgs.callPackage ../../../../../lib/passwords.nix {};
in

{
  networking.wireless = {
    enable = true;

    extraConfig = ''
      ctrl_interface=/run/wpa_supplicant
      ctrl_interface_group=wheel
    '';

    networks = {
      # Home networks
      Centar = {
        psk = passwords.get-password "Wifi/Centar";
        priority = 1;
      };
      CentarPhone = {
        psk = passwords.get-password "Wifi/CentarPhone";
        priority = 2;
      };
      CentarCar.psk = passwords.get-password "Wifi/CentarCar";

      # Netflix networks
      Netflix = {};

      # Friends' networks
      "PC House2".psk = passwords.get-password "Wifi/PC House2";

      # Hotel networks
      Courtyard_GUEST = {};
    };
  };
}
