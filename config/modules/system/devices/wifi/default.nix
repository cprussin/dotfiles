{ pkgs, ... }:

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
        pskRaw = builtins.extraBuiltins.wpaPassphrase pkgs "Centar";
        priority = 1;
        extraConfig = "bgscan=\"simple:30:-70:3600\"";
      };
      CentarPhone = {
        pskRaw = builtins.extraBuiltins.wpaPassphrase pkgs "CentarPhone";
        priority = 2;
      };
      CentarCar.pskRaw = builtins.extraBuiltins.wpaPassphrase pkgs "CentarCar";

      # Netflix networks
      Netflix = { };

      # Friends' networks
      "PC House2".pskRaw = builtins.extraBuiltins.wpaPassphrase pkgs "PC House2";

      # Hotel networks
      Courtyard_GUEST = { };
    };
  };
}
